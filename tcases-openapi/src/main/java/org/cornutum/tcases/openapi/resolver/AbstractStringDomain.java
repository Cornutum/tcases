//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import static org.cornutum.tcases.util.CollectionUtils.restOf;

import org.cornutum.regexpgen.RegExpGen;
import org.cornutum.regexpgen.Bounds;
import org.cornutum.regexpgen.RandomGen;
import org.cornutum.regexpgen.js.Parser;
import org.cornutum.regexpgen.random.RandomBoundsGen;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
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
  public void setExcludedStrings( Set<String> excluded)
    {
    setExcluded( excluded);
    }
  
  /**
   * Returns the length of the given value.
   */
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
   * Returns the set of patterns that values in this domain must <U>NOT</U> match.
   */
  protected List<String> getNotMatching()
    {
    return notMatching_;
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value);
    }

  /**
   * Returns a new random string.
   */
  protected String newValue( ResolverContext context)
    {
    return newValue( context, new PatternResolver( context));
    }

  /**
   * Returns a new random string.
   */
  protected String newValue( ResolverContext context, PatternResolver patternResolver)
    {
    return
      context.tryUntil(
        () ->
        Optional.of( newValue( context, getLengthRange().selectValue( context)))
        .filter( value -> patternResolver.matchesAll( value)));
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected abstract String newValue( ResolverContext context, int length);
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  protected Stream<String> candidates( ResolverContext context)
    {
    return Stream.generate( () -> newValue( context));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
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

      OptionalInt generatedBy =
        IntStream.range( 0, matching.size())
        .filter( i -> {
            try
              {
                Parser.parseRegExp( matching.get(i));
                return true;
              }
            catch( IllegalArgumentException e)
              {
                return false;
              }
          })
        .findFirst();
    
      if( generatedBy.isPresent())
        {
        generatedBy_ = matching.get( generatedBy.getAsInt());
        filtering_ = toPatterns( context, restOf( matching, generatedBy.getAsInt()));
        matching_ = toPatterns( null, matching);
        }
      else
        {
        generatedBy_ = matching.stream().findFirst().orElse( null);
        filtering_ = emptyList();
        matching_ = toPatterns( context, matching);
        }

      notMatching_ = toPatterns( context, notMatching);
      }

    /**
     * Returns if the given value matches all pattern requirements.
     */
    public boolean matchesAll( String value)
      {
      return matches( value, matching_, notMatching_);
      }

    /**
     * Returns if a generated match matches all remaining pattern requirements.
     */
    public boolean matchesRemaining( String match)
      {
      return matches( match, filtering_, notMatching_);
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
          generator_ = Parser.parseRegExp( generatedBy_);
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
     * Returns a generated match to all pattern requirements {@link #getMatching matching pattern}.
     * Returns <CODE>Optional.empty()</CODE> if no generated match is possible.
     */
    public Optional<String> generatedMatch( LengthDomain length)
      {
      return
        Optional.ofNullable( getGenerator())
        .map( generator -> {
          return
            context_.tryUntil(
              () ->
              Optional.of( generator.generate( random_, new Bounds( length.getMin(), length.getMax())))
              .filter( value -> matchesRemaining( value)));
          });
      }

    private final ResolverContext context_;
    private final List<Pattern> matching_;
    private final List<Pattern> notMatching_;
    private final List<Pattern> filtering_;
    private String generatedBy_;
    private RegExpGen generator_;
    private RandomGen random_;
    }
  }

