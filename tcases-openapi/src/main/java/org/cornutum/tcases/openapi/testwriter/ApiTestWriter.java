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
 * Identifies a {@link TestWriter} implementation
 */
@Documented
@Retention( RUNTIME)
public @interface ApiTestWriter
  {
  String name();
  String targetClass() default "org.cornutum.tcases.openapi.testwriter.JavaTestTarget";
  }
