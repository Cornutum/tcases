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
  public static final AnnotatedMatcher annotatedMatcher = new AnnotatedMatcher();
  public static final FunctionInputDefMatcher functionInputDefMatcher = new FunctionInputDefMatcher();
  public static final FunctionTestDefMatcher functionTestDefMatcher = new FunctionTestDefMatcher();
  public static final SystemInputDefMatcher systemInputDefMatcher = new SystemInputDefMatcher();
  public static final SystemTestDefMatcher systemTestDefMatcher = new SystemTestDefMatcher();
  public static final TestCaseMatcher testCaseMatcher = new TestCaseMatcher();
  public static final VarBindingMatcher varBindingMatcher = new VarBindingMatcher();
  public static final VarDefMatcher varDefMatcher = new VarDefMatcher();
  public static final VarValueDefMatcher varValueDefMatcher = new VarValueDefMatcher();
  }
