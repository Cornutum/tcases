//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.parser;

import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.VarValueDef;
import org.cornutum.tcases.annotation.Has;
import org.cornutum.tcases.annotation.Value;
import org.cornutum.tcases.annotation.Var;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Given a Java Bean classes annotated with Tcases annotations, created an IVarDef
 */
public class VarValueDefReader {

  private VarValueDefReader() {
  }

  /**
   * Creates values for a var field depending on the type and annotations.
   */
  @SuppressWarnings("unchecked")
  static List<VarValueDef> getVarValueDefs(Field field) {
    List<VarValueDef> varValueDefs;
    Var varAnnotation = field.getAnnotation(Var.class);
    if (field.getType().isPrimitive()) {
      throw new UnsupportedOperationException("TODO implement support of primitive types");
    } else if (field.getType() == Boolean.class) {
      varValueDefs = VarValueDefReader.getVarValueDefsForBoolean(varAnnotation);
    } else if (field.getType().isEnum()) {
      varValueDefs = VarValueDefReader.getVarValueDefsForEnumField((Class<? extends Enum>) field.getType(), varAnnotation);
    } else {
      if (varAnnotation != null && varAnnotation.values().length > 0) {
        varValueDefs = Arrays.stream(varAnnotation.values())
                .map(varValue -> VarValueDefReader.createVarValueDef(varValue.value(), varValue))
                .collect(Collectors.toList());
      } else {
        throw new IllegalStateException("@Var field must be enum, boolean or define values");
      }
    }
    return varValueDefs;
  }

  /**
   * Creates VarValue for each enum constant, with additional properties if the Var annotation provides any.
   */
  static List<VarValueDef> getVarValueDefsForEnumField(Class<? extends Enum> enumClass, Var varAnnotation) {
    List<VarValueDef> varValueDefs = new ArrayList<>();
    for (Field enumField : enumClass.getFields()) {
      VarValueDef value = null;
      if (varAnnotation.values().length > 0) {
        for (Value varValue : varAnnotation.values()) {
          try {
            enumClass.getField(varValue.value());
          } catch (NoSuchFieldException e) {
            throw new IllegalStateException("@Value value '" + varValue.value()
                    + "' not a known key in Enum " + enumClass.getName());
          }
          if (enumField.getName().equals(varValue.value())) {
            value = VarValueDefReader.createVarValueDef(enumField.getName(), varValue);
            break;
          }
        }
      }
      if (value == null) {
        value = new VarValueDef(enumField.getName(), VarValueDef.Type.VALID);
      }
      varValueDefs.add(value);
    }
    return varValueDefs;
  }

  /**
   * Creates a true and a false VarValue, with additional properties if the Var annotation provides any.
   */
  private static List<VarValueDef> getVarValueDefsForBoolean(Var varAnnotation) {
    List<VarValueDef> varValues = new ArrayList<>();
    for (String boolname : Arrays.asList("true", "false")) {
      VarValueDef value = null;
      if (varAnnotation != null && varAnnotation.values().length > 0) {
        for (Value varValue : varAnnotation.values()) {
          if (!"true".equalsIgnoreCase(varValue.value())
                  && !"false".equalsIgnoreCase(varValue.value())) {
            throw new IllegalStateException("@Value value '" + varValue.value() + "' not a valid Boolean value");
          }
          if (boolname.equalsIgnoreCase(varValue.value())) {
            value = VarValueDefReader.createVarValueDef(boolname, varValue);
            break;
          }
        }
      }
      if (value == null) {
        value = new VarValueDef(boolname, VarValueDef.Type.VALID);
      }
      varValues.add(value);
    }
    return varValues;
  }

  static VarValueDef createVarValueDef(String name, Value varValue) {
    VarValueDef varValueDef = new VarValueDef(name, typeOf(varValue));
    for (Has has : varValue.having()) {
      varValueDef.setAnnotation(has.name(), has.value());
    }
    varValueDef.addProperties(varValue.properties());
    varValueDef.setCondition(ConditionReader.getCondition(varValue.when(), varValue.whenNot()));
    return varValueDef;
  }

  /**
   * returns the Tcases Type given attributes of a Value annotation.
   */
  private static VarValueDef.Type typeOf(Value varValue) {
    if (varValue.type() == TestCase.Type.FAILURE) {
      return VarValueDef.Type.FAILURE;
    }
    if (varValue.once()) {
      return VarValueDef.Type.ONCE;
    }
    return VarValueDef.Type.VALID;
  }

}
