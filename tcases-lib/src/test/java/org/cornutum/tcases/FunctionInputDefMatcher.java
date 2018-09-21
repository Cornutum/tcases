//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link FunctionInputDef} objects.
 */
public class FunctionInputDefMatcher extends BaseCompositeMatcher<FunctionInputDef>
  {
  /**
   * Creates a new FunctionInputDefMatcher instance.
   */
  public FunctionInputDefMatcher( FunctionInputDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", FunctionInputDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "vars", FunctionInputDef::getVarDefs).matches( visitsMembersMatching( VarDefMatcher::new)));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
