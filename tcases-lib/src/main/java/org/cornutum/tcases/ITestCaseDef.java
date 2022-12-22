//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Iterator;
import java.util.Set;

/**
 * Supplies a definition of a {@link TestCase test case}.
 */
public interface ITestCaseDef extends Comparable<ITestCaseDef>
  {
  /**
   * Returns the (optional) id for this test case.
   */
  public Integer getId();

  /**
   * Returns the (optional) name for this test case.
   */
  public String getName();

  /**
   * Returns the variables currently bound in this test case.
   */
  public Iterator<VarDef> getVars();

  /**
   * Returns the current value binding for the given input variable.
   */
  public VarValueDef getValue( VarDef var);

  /**
   * Returns true if the given value is currently bound to the "not applicable" value.
   */
  public boolean isNA( VarDef var);

  /**
   * Returns the variable bound to an invalid value, if any.
   */
  public VarDef getInvalidVar();

  /**
   * Returns the properties of this test case.
   */
  public Set<String> getProperties();
  
  }
