//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.ClassCompositeMatcher;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link ApiKeyDef} objects.
 */
public class ApiKeyDefMatcher extends ClassCompositeMatcher<ApiKeyDef>
  {
  /**
   * Creates a new ApiKeyDefMatcher instance.
   */
  public ApiKeyDefMatcher( ApiKeyDef expected)
    {
    super( ApiKeyDef.class, expected);
    expectThat( valueOf( "location", ApiKeyDef::getLocation).matches( Matchers::equalTo));
    expectThat( valueOf( "name", ApiKeyDef::getName).matches( Matchers::equalTo));
    }
  }
