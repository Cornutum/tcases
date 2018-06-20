//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.reader;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.FunctionInputDef;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.annotation.Function;
import org.cornutum.tcases.annotation.Has;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import static org.cornutum.tcases.annotation.reader.AnnotatedVarDefReader.readVarDef;

/**
 * Given a Java Bean classes annotated with Tcases annotations, created a SystemInputDef
 */
public class AnnotatedFunctionDefReader {

  private AnnotatedFunctionDefReader() {
  }

  /**
   * create System Def from given classes as FunctionDefs with given name.
   */
  public static SystemInputDef readSystemDef(String systemName, Class<?>... functionDefClass) {
    SystemInputDef inputDef = new SystemInputDef(systemName);
    for (Class<?> annotatedClass: functionDefClass) {
      inputDef.addFunctionInputDef(readFunctionInputDef(annotatedClass));
    }
    return inputDef;
  }

  /**
   * create FunctionInputDef from given annotated class.
   */
  static FunctionInputDef readFunctionInputDef(Class<?> annotatedClass) {
    Function functionAnnotation = annotatedClass.getAnnotation(Function.class);
    FunctionInputDef functionDef
            = new FunctionInputDef(readFunctionDefName(annotatedClass, functionAnnotation));
    if (functionAnnotation != null) {
      for (Has has : functionAnnotation.having()) {
        functionDef.setAnnotation(has.name(), has.value());
      }
    }

    for (Field field: annotatedClass.getDeclaredFields()) {
      if (!Modifier.isStatic(field.getModifiers())) {
        functionDef.addVarDef(readVarDef(field));
      }
    }
    return functionDef;
  }

  /**
   * returns the name given wth the Function annotation, else the SimpleClassName.
   */
  private static String readFunctionDefName(Class<?> annotatedClass, Function functionAnnotation) {
    String functionName;
    if (functionAnnotation == null || StringUtils.isBlank(functionAnnotation.value())) {
      functionName = annotatedClass.getSimpleName();
    } else {
      functionName = functionAnnotation.value();
    }
    return functionName;
  }
}
