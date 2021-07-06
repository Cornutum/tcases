//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.ClassCompositeMatcher;

/**
 * A composite matcher for {@link HttpBasicDef} objects.
 */
public class HttpBasicDefMatcher extends ClassCompositeMatcher<HttpBasicDef>
  {
  /**
   * Creates a new HttpBasicDefMatcher instance.
   */
  public HttpBasicDefMatcher( HttpBasicDef expected)
    {
    super( HttpBasicDef.class, expected);
    }
  }
