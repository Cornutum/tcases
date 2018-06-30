//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Collection;
import java.util.regex.Pattern;

/**
 * Defines utility methods for constructing test definitions.
 *
 */
public abstract class DefUtils
  {
  /**
   * Returns true if the given string is a valid identifier.
   */
  public static boolean isIdentifier( String id)
    {
    return id != null && identifierRegex_.matcher( id).matches();
    }  

  /**
   * Translates the given identifier path name into a corresponding
   * sequence of identifiers.
   */
  public static String[] toPath( String pathName)
    {
    return
      pathName == null
      ? null
      : pathName.split( "\\.", -1);
    }
  
  /**
   * Throws an exception if the given string is not a valid identifier.
   */
  public static void assertIdentifier( String id) throws IllegalArgumentException
    {
    if( !isIdentifier( id))
      {
      throw
        new IllegalArgumentException
        ( (id==null? "null" : ("\"" + String.valueOf( id) + "\""))
          + " is not a valid identifier");
      }
    }

  /**
   * Returns true if the given string is a valid identifier.
   */
  public static boolean isVarValue( String val)
    {
    return val != null && varValueRegex_.matcher( val).matches();
    }

  /**
    * Throws an exception if the given string is not a valid variable value.
    */
  public static void assertVarValue( String val) throws IllegalArgumentException
    {
    if( !isVarValue( val))
      {
      throw
        new IllegalArgumentException
        ( (val==null? "null" : ("\"" + String.valueOf( val) + "\""))
          + " is not a valid variable value");
      }
    }


    /**
   * Throws an exception if the given string is not a valid identifier path name.
   */
  public static void assertPath( String pathName) throws IllegalArgumentException
    {
    String ids[] = toPath( pathName);
    for( int i = 0; i < ids.length; i++)
      {
      assertIdentifier( ids[i]);
      }
    }
  
  /**
   * Throws an exception if any member of the given set of properties is not a valid identifier.
   */
  public static void assertPropertyIdentifiers( Collection<String> properties) throws IllegalArgumentException
    {
    if( properties != null)
      {
      for( String property : properties)
        {
        assertIdentifier( property);
        }
      }
    }

  private static final Pattern identifierRegex_ = Pattern.compile( "[\\w\\-]+");
  private static final Pattern varValueRegex_ = Pattern.compile( "([^\\p{Cntrl}]|\\s)*");
  }

