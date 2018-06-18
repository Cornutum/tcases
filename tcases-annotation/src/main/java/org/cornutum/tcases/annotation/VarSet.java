package org.cornutum.tcases.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface VarSet {

  Value[] values() default {};

  String tag() default "arg"; // Input type from docs, TODO: not sure what to do with it

  String[] when() default {};
  String[] whenNot() default {};
}
