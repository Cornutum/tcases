package org.cornutum.tcases.annotation;

import org.cornutum.tcases.TestCase;

import java.lang.annotation.*;

/**
 * Container annotation single value for a variable within a @Var annotation.
 * Can be used on Fields of Type Enum, String, Boolean (Maybe more in the future).
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Value {

  /**
   * the actual value, can represent a boolean or an enum value as well.
   */
  String value();

  TestCase.Type type() default TestCase.Type.SUCCESS;

  /**
   * if value is used, testcase includes these properties
   */
  String[] properties() default {};

  /**
   * use value only if testcase has given properties
   */
  String[] when() default {};
  /**
   * use value only if testcase does not have given properties
   */
  String[] whenNot() default {};

  /**
   * hint to use this value only once in all vaid testcases
   */
  boolean once() default false;

  Has[] having() default {};
}
