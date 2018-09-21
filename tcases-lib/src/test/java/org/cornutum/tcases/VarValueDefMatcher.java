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
import org.hamcrest.Matchers;

import java.util.Iterator;

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

    expectThat( valueOf( "name", VarValueDef::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "type", VarValueDef::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "condition", VarValueDef::getCondition).matches( Matchers::equalTo));
    expectThat( valueOf( "properties", this::getProperties).matches( Composites::visitsMembers));
    expectThat( matches( AnnotatedMatcher::new));
    }

  /**
   * Returns an Iterator that visits all property identifers for a VarValueDef.
   */
  private Iterator<String> getProperties( VarValueDef varValueDef)
    {
    return varValueDef.getProperties().getProperties();
    }
  }
