//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.util.Characters;
import static org.cornutum.tcases.util.CollectionUtils.restOf;

import org.cornutum.regexpgen.RegExpGen;
import org.cornutum.regexpgen.RandomGen;
import org.cornutum.regexpgen.js.Provider;
import org.cornutum.regexpgen.random.RandomBoundsGen;

import java.util.AbstractMap.SimpleEntry;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

/**
 * Defines a set of string values that can be used by a request.
 */
public abstract class AbstractStringDomain extends SequenceDomain<String>
  {
  /**
   * Creates a new AbstractStringDomain instance.
   */
  protected AbstractStringDomain( int maxLength, Characters chars)
    {
    super( maxLength);
    chars_ = chars;
    }

  /**
   * Returns the set of characters allowed in values for this domain.
   */
  public Characters getCharacters()
    {
    return chars_;
    }

  /**
   * Changes the values excluded from this domain.
   */
  @Override
  public void setExcludedStrings( Set<String> excluded)
    {
    setExcluded( excluded);
    }
  
  /**
   * Returns the length of the given value.
   */
  @Override
  protected int getLength( String value)
    {
    return value.length();
    }

  /**
   * Changes the set of patterns that values in this domain must match.
   */
  public void setMatching( List<String> patterns)
    {
    matching_ = patterns;
    }

  /**
   * Changes the set of patterns that values in this domain must match.
   */
  public void setMatching( String... patterns)
    {
    setMatching( Arrays.asList( patterns));
    }

  /**
   * Returns the set of patterns that values in this domain must match.
   */
  protected List<String> getMatching()
    {
    return matching_;
    }

  /**
   * Changes the set of patterns that values in this domain must <U>NOT</U> match.
   */
  public void setNotMatching( List<String> patterns)
    {
    notMatching_ = patterns;
    }

  /**
   * Changes the set of patterns that values in this domain must <U>NOT</U> match.
   */
  public void setNotMatching( String... patterns)
    {
    setNotMatching( Arrays.asList( patterns));
    }

  /**
   * Returns the set of patterns that values in this domain must <U>NOT</U> match.
   */
  protected List<String> getNotMatching()
    {
    return notMatching_;
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  @Override
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value, getFormat());
    }

  /**
   * Returns a random sequence of possible members of this domain matching all pattern requirements.
   */
  protected Stream<String> matchingValues( ResolverContext context, PatternResolver patternResolver)
    {
    return Stream.generate( () -> {
      return
        context.tryUntil(
          newValues( context)
          .map( value -> Optional.of( value).filter( v -> patternResolver.matchesAll( v))));
      });
    }

  /**
   * Generates a random sequence of possible members of this domain matching all pattern requirements.
   */
  protected Stream<String> generateMatchingValues( ResolverContext context, PatternResolver patternResolver)
    {
    return
      patternResolver.generatedMatches( getLengthRange())
      .orElse( matchingValues( context, patternResolver));
    }

  /**
   * Returns a random sequences of possible members of this domain.
   */
  protected Stream<String> newValues( ResolverContext context)
    {
    return Stream.generate( () -> newValue( context, getLengthRange().selectValue( context)));
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected abstract String newValue( ResolverContext context, int length);
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  @Override
  protected Stream<String> candidates( ResolverContext context)
    {
    PatternResolver patternResolver = new PatternResolver( context);
    patternResolver.patternInfeasible( getLengthRange())
      .ifPresent( pattern -> {
        throw
          new ResolverSkipException(
            context.getLocation(),
            String.format( "Can't match pattern=%s with length=%s", pattern, getLengthRange()));
        });

    return matchingCandidates( context, patternResolver);
    }
  
  /**
   * Returns a random sequence of possible members of this domain matching all pattern requirements.
   */
  protected Stream<String> matchingCandidates( ResolverContext context, PatternResolver patternResolver)
    {
    return matchingValues( context, patternResolver);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  @Override
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && getCharacters().allowed( value)
      && matchesPatterns( value);
    }

  /**
   * Returns if the given value matches the given pattern requirements.
   */
  protected boolean matchesPatterns( String value)
    {
    return new PatternResolver().matchesAll( value);
    }
  
  private final Characters chars_;
  private List<String> matching_ = emptyList();
  private List<String> notMatching_ = emptyList();

  /**
   * Resolves string data values by matching regular expressions.
   */
  protected class PatternResolver
    {
    /**
     * Creates a new PatternResolver instance.
     */
    public PatternResolver()
      {
      this( null);
      }
  
    /**
     * Creates a new PatternResolver instance.
     */
    public PatternResolver( ResolverContext context)
      {
      context_ = context;

      List<String> matching = getMatching();
      List<String> notMatching = getNotMatching();

      List<SimpleEntry<Integer,RegExpGen>> generatorsIndexed =
        IntStream.range( 0, matching.size())
        .mapToObj( i -> Optional.ofNullable( generatorFor( matching.get(i))).map( gen -> new SimpleEntry<Integer,RegExpGen>( i, gen)).orElse( null))
        .filter( Objects::nonNull)
        .sorted( Comparator.nullsLast( Comparator.comparing( SimpleEntry::getValue)))
        .collect( toList());

      generators_ = generatorsIndexed.stream().map( SimpleEntry::getValue).collect( toList());
      
      Optional<Integer> generatedBy = generatorsIndexed.stream().findFirst().map( SimpleEntry::getKey);
      if( generatedBy.isPresent())
        {
        generatedBy_ = matching.get( generatedBy.get());
        filtering_ = toPatterns( context, restOf( matching, generatedBy.get()));
        matchingPatterns_ = toPatterns( null, matching);
        }
      else
        {
        generatedBy_ = matching.stream().findFirst().orElse( null);
        filtering_ = emptyList();
        matchingPatterns_ = toPatterns( context, matching);
        }

      notMatchingPatterns_ = toPatterns( context, notMatching);
      }

    /**
     * Returns if the given value matches all pattern requirements.
     */
    public boolean matchesAll( String value)
      {
      return matches( value, matchingPatterns_, notMatchingPatterns_);
      }

    /**
     * Returns if a generated match matches all remaining pattern requirements.
     */
    public boolean matchesRemaining( String match)
      {
      return matches( match, filtering_, notMatchingPatterns_);
      }

    /**
     * Returns if the given value matches the given pattern requirements.
     */
    private boolean matches( String value, List<Pattern> matching, List<Pattern> notMatching)
      {
      return
        matching.stream().allMatch( pattern -> pattern.matcher( value).find())
        && notMatching.stream().noneMatch( pattern -> pattern.matcher( value).find());
      }

    /**
     * If all pattern requirements are feasible for strings with the given length domain,
     * returns <CODE>Optional.empty()</CODE>; otherwise, returns an infeasible pattern.
     */
    public Optional<String> patternInfeasible( LengthDomain length)
      {
      return
        generators_.stream()
        .filter( gen -> !gen.getLength().intersects( length.getMin(), length.getMax()))
        .findFirst()
        .map( gen -> gen.getSource());
      }

    /**
     * Converts the given regular expressions into Pattern instances.
     */
    private List<Pattern> toPatterns( ResolverContext context, List<String> regexps)
      {
      return
        regexps.stream()
        .map( regexp -> {
          try
            {
            return Pattern.compile( regexp);
            }
          catch( PatternSyntaxException e)
            {
            if( context != null)
              {
              context.error( String.format( "Can't use pattern='%s' to select values -- %s", regexp, e.getMessage()), "Ignoring this pattern");
              }
            return null;
            }
          })
        .filter( Objects::nonNull)
        .collect( toList());
      }

    /**
     * Returns the generator used to provide matching strings.
     */
    public RegExpGen getGenerator()
      {
      if( generator_ == null && generatedBy_ != null)
        {
        try
          {
          generator_ = Provider.forEcmaScript().matching( generatedBy_);
          generator_.getOptions().setAnyPrintableChars( getCharacters().filtered( Characters.Ascii.chars()).get());
          random_ = new RandomBoundsGen( context_.getRandom());
          }
        catch( IllegalArgumentException e)
          {
          if( context_ != null)
            {
            context_.error( String.format( "Can't use pattern='%s' to generate values -- %s", generatedBy_, e.getMessage()), "Ignoring this pattern");
            }
          }
        finally
          {
          generatedBy_ = null;
          }
        }

      return generator_;
      }

    /**
     * Returns a sequence of generated matches to all pattern requirements {@link #getMatching matching pattern}.
     * Returns <CODE>Optional.empty()</CODE> if no generated match is possible.
     */
    public Optional<Stream<String>> generatedMatches( LengthDomain length)
      {
      return
        Optional.ofNullable( getGenerator())
        .map( generator -> {
          return
            Stream.generate( () -> {
              return
                context_.tryUntil( () -> {
                  return
                    Optional.of( generator.generate( random_, length.getMin(), length.getMax()))
                    .filter( value -> matchesRemaining( value));
                  });
              });
          });
      }

    /**
     * Returns a RegExpGen for the given regular expression.
     */
    private RegExpGen generatorFor( String regexp)
      {
      try
        {
        return Provider.forEcmaScript().matching( regexp);
        }
      catch( IllegalArgumentException e)
        {
        return null;
        }
      }

    private final ResolverContext context_;
    private final List<RegExpGen> generators_;
    private final List<Pattern> matchingPatterns_;
    private final List<Pattern> notMatchingPatterns_;
    private final List<Pattern> filtering_;
    private String generatedBy_;
    private RegExpGen generator_;
    private RandomGen random_;
    }
  }

