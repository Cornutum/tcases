//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Represents a generated UUID string value for a request case.
 */
public class UuidValue extends StringValue
  {
  /**
   * Creates a new UuidValue instance.
   */
  public UuidValue( String value)
    {
    super( value, "uuid");
    }
  }
