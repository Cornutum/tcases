//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

/**
 * Defines {@link Matcher} instances used to compare Tcases objects.
 */
public abstract class Matchers 
  {
  public static final SystemTestDefMatcher systemTestDefMatcher = new SystemTestDefMatcher();
  public static final FunctionTestDefMatcher functionTestDefMatcher = new FunctionTestDefMatcher();
  public static final TestCaseMatcher testCaseMatcher = new TestCaseMatcher();
  public static final VarBindingMatcher varBindingMatcher = new VarBindingMatcher();
  }
