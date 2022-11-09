//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.tcases.resolve.Schema;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link IVarDef} objects.
 */
public class VarDefMatcher extends BaseCompositeMatcher<IVarDef>
  {
  /**
   * Creates a new VarDefMatcher instance.
   */
  public VarDefMatcher( IVarDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", IVarDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "type", IVarDef::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "condition", IVarDef::getCondition).matches( Matchers::equalTo));
    expectThat( valueOf( "values", IVarDef::getValues).matches( visitsMembersMatching( VarValueDefMatcher::new)));
    expectThat( valueOf( "members", IVarDef::getMembers).matches( visitsMembersMatching( VarDefMatcher::new)));
    expectThat( valueOf( "schema", this::getSchema).matches( SchemaMatcher::new));
    expectThat( matches( AnnotatedMatcher::new));
    }

  private Schema getSchema( IVarDef varDef)
    {
    return
      varDef.getValues() != null
      ? ((VarDef) varDef).getSchema()
      : null;
    }
  }
