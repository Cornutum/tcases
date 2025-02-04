//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import java.lang.annotation.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * Identifies a {@link TestCaseWriter} implementation
 */
@Documented
@Retention( RUNTIME)
public @interface ApiTestCaseWriter
  {
  String name();
  }
