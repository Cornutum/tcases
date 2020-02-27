//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.regex.Pattern;

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
    this( 256);
    }
  
  /**
   * Creates a new AsciiStringDomain instance.
   */
  public AsciiStringDomain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && !nonAsciiPrintable_.matcher( value).find();
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected String newValue( ResolverOptions options, int length)
    {
    StringBuilder value = new StringBuilder();
    for( int i = 0; i < length; i++)
      {
      value.append( asciiPrintable_.charAt( options.getRandom().nextInt( asciiPrintable_.length())));
      }

    return value.toString();
    }

  private final static Pattern nonAsciiPrintable_ = Pattern.compile( "\\P{Print}");

  private static String asciiPrintableChars()
    {
    StringBuilder asciiChars = new StringBuilder();
    for( int codePoint = 0; codePoint < 128; codePoint++)
      {
      asciiChars.appendCodePoint( codePoint);
      }

    return
      nonAsciiPrintable_
      .matcher( asciiChars.toString())
      .replaceAll( "");
    }

  private final static String asciiPrintable_ = asciiPrintableChars();
}
