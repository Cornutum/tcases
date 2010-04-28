//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases;

import java.util.Iterator;

/**
 * Defines an input variable.
 *
 * @version $Revision$, $Date$
 */
public interface IVarDef
  {
  /**
   * Changes the parent of this variable.
   */
  void setParent( IVarDef parent);

  /**
   * If this is member of another variable, returns the parent variable. Otherwise, returns null.
   */
  IVarDef getParent();

  /**
   * Returns the variable name.
   */
  String getName();

  /**
   * Returns the hierarchical path name of this variable.
   */
  String getPathName();

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
   * Returns the condition that defines when values for this variable are applicable.
   */
  ICondition getCondition();


  /**
   * {@link getType Identifies} an input variable that is a formal argument of a function.
   */
  String ARG = "arg";

  /**
   * {@link getType Identifies} an implicit input variable defined by the environment to a function.
   */
  String ENV = "env";

  /**
   * {@link getType Identifies} an implicit input variable defined the internal state of the system.
   */
  String STATE = "state";
  }
