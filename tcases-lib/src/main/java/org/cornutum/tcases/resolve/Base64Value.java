//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Represents a generated base64 address value for a request case.
 */
public class Base64Value extends StringValue
  {
  /**
   * Creates a new Base64Value instance.
   */
  public Base64Value( String value)
    {
    super( value, "byte");
    }
  }
