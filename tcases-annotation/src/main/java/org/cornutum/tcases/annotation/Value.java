package org.cornutum.tcases.annotation;

import org.cornutum.tcases.TestCase;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Value {

  String value();

  TestCase.Type type() default TestCase.Type.SUCCESS;

  String[] properties() default {};
}
