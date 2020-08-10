//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

import java.util.stream.Stream;

/**
 * Defines a set of ASCII string values that can be used by a request.
 */
public class AsciiStringDomain extends AbstractStringDomain
  {
  /**
   * Creates a new AsciiStringDomain instance.
   */
  public AsciiStringDomain()
    {
    this( Characters.ASCII);
    }
  
  /**
   * Creates a new AsciiStringDomain instance.
   */
  public AsciiStringDomain( int maxLength)
    {
    this( maxLength, Characters.ASCII);
    }
  
  /**
   * Creates a new AsciiStringDomain instance.
   */
  public AsciiStringDomain( Characters chars)
    {
    this( 256, chars);
    }
  
  /**
   * Creates a new AsciiStringDomain instance.
   */
  public AsciiStringDomain( int maxLength, Characters chars)
    {
    super( maxLength, chars);

    allowedChars_ =
      chars.filtered( Characters.Ascii.chars())
      .orElseThrow( () -> new ValueDomainException( String.format( "Character set=%s does not accept any ASCII characters", chars)));
    }
  
  /**
   * Returns a random sequence of possible members of this domain matching all pattern requirements.
   */
  protected Stream<String> matchingCandidates( ResolverContext context, PatternResolver patternResolver)
    {
    return generateMatchingValues( context, patternResolver);
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected String newValue( ResolverContext context, int length)
    {
    StringBuilder value = new StringBuilder();
    for( int i = 0; i < length; i++)
      {
      value.append( allowedChars_.charAt( context.getRandom().nextInt( allowedChars_.length())));
      }

    return value.toString();
    }

  private final String allowedChars_;
  }
