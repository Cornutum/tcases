//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.ClassCompositeMatcher;

/**
 * A composite matcher for {@link HttpBearerDef} objects.
 */
public class HttpBearerDefMatcher extends ClassCompositeMatcher<HttpBearerDef>
  {
  /**
   * Creates a new HttpBearerDefMatcher instance.
   */
  public HttpBearerDefMatcher( HttpBearerDef expected)
    {
    super( HttpBearerDef.class, expected);
    }
  }
