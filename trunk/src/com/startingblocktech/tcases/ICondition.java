//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
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
   * A {@link ICondition condition} that is always satisfied by any {@link PropertySet}.
   */
  ICondition ALWAYS = new ICondition()
      {
      public boolean satisfied( PropertySet properties)
        {
        return true;
        }
      };
  }
