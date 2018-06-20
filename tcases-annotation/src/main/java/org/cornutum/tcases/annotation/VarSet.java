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
 * Marker Annotation for a field defining further variability.
 * Annotated field type should be a Java bean containing fields that are either @Var or @VarSets.
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface VarSet {

  String tag() default "arg"; // Input type from docs, TODO: not sure what to do with it

  String[] when() default {};
  String[] whenNot() default {};

  Has[] having() default {};
}
