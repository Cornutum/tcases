//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Random;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Defines a set of string values that can be used by a request.
 */
public class StringDomain extends BaseStringDomain<String>
  {
  /**
   * Creates a new StringDomain instance.
   */
  protected StringDomain()
    {
    this( 256);
    }
  
  /**
   * Creates a new StringDomain instance.
   */
  protected StringDomain( int maxLength)
    {
    super( maxLength);
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
   * Returns a new random string.
   */
  private String newValue( Random random)
    {
    StringBuilder value = new StringBuilder();
    int length = getLengthRange().select( random);
    for( int i = 0; i < length; i++)
      {
      value.append( asciiPrintable_.charAt( random.nextInt( asciiPrintable_.length())));
      }

    return value.toString();
    }
  
  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<String> values( Random random)
    {
    return
      Stream.generate( () -> newValue( random))
      .filter( value -> isNotExcluded( value, getExcluded()));
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value);
    }

  private static String asciiPrintableChars()
    {
    StringBuilder asciiChars = new StringBuilder();
    for( int codePoint = 0; codePoint < 128; codePoint++)
      {
      asciiChars.appendCodePoint( codePoint);
      }

    return
      Pattern.compile( "\\P{Print}")
      .matcher( asciiChars.toString())
      .replaceAll( "");
    }

  private final static String asciiPrintable_ = asciiPrintableChars();
}
