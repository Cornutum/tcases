//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.conditions;

/**
 * Defines the Visitor pattern for {@link ICondition condition} objects.
 *
 * @version $Revision$, $Date$
 */
public interface IConditionVisitor
  {
  void visit( AllOf condition);
  
  void visit( AnyOf condition);
  
  void visit( ContainsAll condition);
  
  void visit( ContainsAny condition);
  
  void visit( IConjunct condition);
  
  void visit( Not condition);
  }
