//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.stream.IntStream;

/**
 * Defines a set of object property name values that can be used by a request.
 */
public class PropertyNameDomain extends AbstractStringDomain
  {
  /**
   * Creates a new PropertyNameDomain instance.
   */
  public PropertyNameDomain()
    {
    this( 1, 16);
    }
  
  /**
   * Creates a new PropertyNameDomain instance.
   */
  public PropertyNameDomain( int minLength, int maxLength)
    {
    super( maxLength);
    setLengthRange( minLength, maxLength);
    setNameChars( asciiAlpha_);
    }

  /**
   * Changes the set of chars allowed in property names for this domain.
   */
  public void setNameChars( String nameChars)
    {
    nameChars_ = nameChars;
    }

  /**
   * Returns the set of chars allowed in property names for this domain.
   */
  public String getNameChars()
    {
    return nameChars_;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( String value)
    {
    return
      super.contains( value)
      && isName( value);
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected String newValue( ResolverOptions options, int length)
    {
    StringBuilder value = new StringBuilder();
    for( int i = 0; i < length; i++)
      {
      value.append( getNameChars().charAt( options.getRandom().nextInt( getNameChars().length())));
      }

    return value.toString();
    }

  /**
   * Returns true is the given value contains only allowed name characters.
   */
  private boolean isName( String value)
    {
    return
      IntStream.range( 0, value.length())
      .allMatch( i -> getNameChars().indexOf( value.charAt(i)) >= 0);
    }

  private String nameChars_;
  
  private final static String asciiAlpha_ = "abcdefghijklmnopqrstuvwxyz";
}
