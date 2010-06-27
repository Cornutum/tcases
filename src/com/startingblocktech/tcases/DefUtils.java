//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import java.util.Collection;
import java.util.regex.Pattern;

/**
 * Defines utility methods for constructing test definitions.
 *
 * @version $Revision$, $Date$
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
   * Throws an exception if any member of the given set of properties is not a valid identifier.
   */
  public static void assertPropertyIdentifiers( Collection<String> properties) throws IllegalArgumentException
    {
    if( properties != null)
      {
      try
        {
        for( String property : properties)
          {
          assertIdentifier( property);
          }
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( "Invalid property set", e);
        }
      }
    }

  private static final Pattern identifierRegex_ = Pattern.compile( "[\\w\\-]+");
  }

