//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marker Annotation for Testcase field that has values.
 * Must either contain @Value annotations, or have implicit values (Enum or Boolean)
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Var {

  Value[] values() default {}; // For Enum / Boolean, the values add properties to the value Defs.

  String tag() default "arg"; // Input type from docs, TODO: not sure what to do with it

  String[] when() default {};
  String[] whenNot() default {};

  Has[] having() default {};
}
