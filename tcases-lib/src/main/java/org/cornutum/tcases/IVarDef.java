//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Iterator;

import org.cornutum.tcases.conditions.ICondition;

/**
 * Defines an input variable.
 *
 */
public interface IVarDef extends IConditional, IAnnotated, Comparable<IVarDef>
  {
  /**
   * Defines an ordering of variable definitions.
   */
  interface Position extends Comparable<Position> {}
  
  /**
   * Changes the parent of this variable.
   */
  void setParent( VarSet parent);

  /**
   * If this is member of another variable, returns the parent variable. Otherwise, returns null.
   */
  VarSet getParent();

  /**
   * Returns the position of this variable definition.
   */
  Position getPosition();

  /**
   * Changes the sequence number of this variable.
   */
  void setSeqNum( int seqNum);

  /**
   * Returns the sequence number of this variable.
   */
  int getSeqNum();

  /**
   * Returns the variable name.
   */
  String getName();

  /**
   * Returns the hierarchical path name of this variable.
   */
  String getPathName();

  /**
   * Returns the effective condition that defines when this variable is applicable,
   * based on the conditions for this variable and all of its ancestors.
   */
  ICondition getEffectiveCondition();

  /**
   * Returns the type identifier for this variable.
   */
  String getType();

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  Iterator<IVarDef> getMembers();

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  Iterator<VarValueDef> getValues();

  /**
   * Returns the descendant variable with the given name path, relative to this variable.
   */
  IVarDef find( String... path);

  /**
   * Returns the condition that defines when values for this variable are applicable.
   */
  @Override
  ICondition getCondition();

  @Override
  default int compareTo( IVarDef other)
    {
    return getPosition().compareTo( other.getPosition());
    }

  /**
   * {@link #getType Identifies} an input variable that is a formal argument of a function.
   */
  String ARG = "arg";

  /**
   * {@link #getType Identifies} an implicit input variable defined by the environment to a function.
   */
  String ENV = "env";

  /**
   * {@link #getType Identifies} an implicit input variable defined by the internal state of the system.
   */
  String STATE = "state";
  }
