//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

/**
 * A condition that evaluates a set of test case properties.
 *
 * @version $Revision$, $Date$
 */
public interface ICondition
  {
  /**
   * Returns true if this condition is satisfied by the given test case properties.
   */
  boolean satisfied( PropertySet properties);

  /**
   * Returns true if this condition is compatible with the given test case properties.
   * A condition is <em>"compatible"</em> with these properties if it is already satisfied
   * or if it could be satisfied with the addition of more properties.
   */
  boolean compatible( PropertySet properties);

  /**
   * A {@link ICondition condition} that is always satisfied by any {@link PropertySet}.
   */
  ICondition ALWAYS = new ICondition()
      {
      public boolean satisfied( PropertySet properties)
        {
        return true;
        }
      public boolean compatible( PropertySet properties)
        {
        return true;
        }
      public String toString()
        {
        return "ALWAYS";
        }
      };
  }
