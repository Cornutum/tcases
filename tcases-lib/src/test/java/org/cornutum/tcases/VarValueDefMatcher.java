//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.hamcrest.Composites;
import org.cornutum.tcases.resolve.SchemaMatcher;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link VarValueDef} objects.
 */
public class VarValueDefMatcher extends BaseCompositeMatcher<VarValueDef>
  {
  /**
   * Creates a new VarValueDefMatcher instance.
   */
  public VarValueDefMatcher( VarValueDef expected)
    {
    super( expected);

    expectThat( valueOf( "name", VarValueDef::getExternalName).matches( Matchers::equalTo));
    expectThat( valueOf( "type", VarValueDef::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "condition", VarValueDef::getCondition).matches( Matchers::equalTo));
    expectThat( valueOf( "properties", VarValueDef::getProperties).matches( Composites::containsMembers));
    expectThat( valueOf( "schema", VarValueDef::getSchema).matches( SchemaMatcher::new));
    expectThat( matches( AnnotatedMatcher::new));
    }
  }
