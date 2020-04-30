//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

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
    super( maxLength, new PropertyName());
    setLengthRange( minLength, maxLength);
    }

  /**
   * Changes the set of chars allowed in property names for this domain.
   */
  public void setNameChars( String nameChars)
    {
    getPropertyName().setNameChars( nameChars);
    }

  /**
   * Returns the set of chars allowed in property names for this domain.
   */
  public String getNameChars()
    {
    return getPropertyName().getNameChars();
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
  protected String newValue( ResolverContext context, int length)
    {
    StringBuilder value = new StringBuilder();
    for( int i = 0; i < length; i++)
      {
      value.append( getNameChars().charAt( context.getRandom().nextInt( getNameChars().length())));
      }

    return value.toString();
    }

  private PropertyName getPropertyName()
    {
    return (PropertyName) getCharacters();
    }
  
  /**
   * Returns true is the given value contains only allowed name characters.
   */
  private boolean isName( String value)
    {
    return getPropertyName().allowed( value);
    }

  /**
   * Defines the set of characters allowed in an object property name.
   */
  public static class PropertyName extends Characters.Base
    {
    /**
     * Return the name of this set of characters.
     */
    public String getName()
      {
      return "PROPERTY_NAME";
      }

    /**
     * Changes the set of chars allowed in property names.
     */
    public void setNameChars( String nameChars)
      {
      nameChars_ = nameChars;
      }

    /**
     * Returns the set of chars allowed in property names.
     */
    public String getNameChars()
      {
      return nameChars_;
      }
    
    /**
     * Returns true if and only if the given character is allowed.
     */
    public boolean allowed( char c)
      {
      return getNameChars().indexOf( c) >= 0;
      }

    // By default, use only lower-case alphabetic chars.
    private String nameChars_ = "abcdefghijklmnopqrstuvwxyz";
    }
  }
