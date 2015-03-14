//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

/**
 * Defines the Visitor pattern for {@link ICondition condition} objects.
 *
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
