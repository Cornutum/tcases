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
  /**
   * Defines the name used to locate this {@link TestWriter} implementation at runtime
   */
  String name();

  /**
   * Defines the fully-qualified class name of the {@link TestTarget} implementation required for this {@link TestWriter}.
   */
  String targetClass() default "org.cornutum.tcases.openapi.testwriter.JavaTestTarget";
  }
