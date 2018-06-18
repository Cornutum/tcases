//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.parser;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.annotation.*;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AnnotationReader
  {
    private AnnotationReader() {
    }

    public static SystemInputDef createDef(String systemName, Class<?>... functionDefClass) {
      SystemInputDef inputDef = new SystemInputDef(systemName);

      for (Class<?> annotatedClass: functionDefClass) {
        Function[] annotations = annotatedClass.getAnnotationsByType(Function.class);
        assert annotations.length == 1;
        Function functionAnnotation = annotations[0];
        String functionName = StringUtils.isBlank(functionAnnotation.value()) ? annotatedClass.getSimpleName(): functionAnnotation.value();
        FunctionInputDef functionDef = new FunctionInputDef(functionName);
        inputDef.addFunctionInputDef(functionDef);
        for (Field field: annotatedClass.getDeclaredFields()) {
          for (IVarDef varDef : createVarDefs(field)) {
            functionDef.addVarDef(varDef);
          }
        }
      }

      return inputDef;
    }

    private static List<IVarDef> createVarDefs(Field field) {
      List<IVarDef> varDefs = new ArrayList<>();
      Var[] varAnnotations = field.getAnnotationsByType(Var.class);

      if (varAnnotations.length > 0) {
        // TODO: Assert length must be 1?
        for (Var varAnnotation : varAnnotations) {
          VarDef varDef = new VarDef(field.getName());
          if (field.getType().isPrimitive()) {
            throw new UnsupportedOperationException("TODO implement support of primitive types");
          } else if (field.getType() == Boolean.class) {
            if (varAnnotation.values().length > 0) {
              for (String boolname : Arrays.asList("true", "false")) {
                boolean isVarDefCreatedByAnnotation = false;
                for (Value varValue : varAnnotation.values()) {
                  if (!"true".equalsIgnoreCase(varValue.value())
                          && !"false".equalsIgnoreCase(varValue.value())) {
                    throw new IllegalStateException("@Value value '" + varValue.value() + "' not a valid Boolean value");
                  }
                  if (boolname.equalsIgnoreCase(varValue.value())) {
                    varDef.addValue(new VarValueDef(boolname, typeOf(varValue)));
                    isVarDefCreatedByAnnotation = true;
                    break;
                  }
                }
                if (!isVarDefCreatedByAnnotation) {
                  varDef.addValue(new VarValueDef(boolname, VarValueDef.Type.VALID));
                }
              }
            }
          } else if (field.getType().isEnum()) {
            for (Field enumField : field.getType().getFields()) {
              boolean isVarDefCreatedByAnnotation = false;
              if (varAnnotation.values().length > 0) {
                for (Value varValue : varAnnotation.values()) {
                  try {
                    field.getType().getField(varValue.value());
                  } catch (NoSuchFieldException e) {
                    throw new IllegalStateException("@Value value '" + varValue.value() + "' not a known key in Enum " + field.getType().getName());
                  }
                  if (enumField.getName().equals(varValue.value())) {
                    varDef.addValue(new VarValueDef(enumField.getName(), typeOf(varValue)));
                    isVarDefCreatedByAnnotation = true;
                    break;
                  }
                }
              }
              if (!isVarDefCreatedByAnnotation) {
                varDef.addValue(new VarValueDef(enumField.getName(), VarValueDef.Type.VALID));
              }
            }
          } else {
            if (varAnnotation.values().length > 0) {
              for (Value varValue : varAnnotation.values()) {
                varDef.addValue(new VarValueDef(varValue.value(), typeOf(varValue)));
              }
            } else {
              throw new IllegalStateException("@Var field must be enum or define values");
            }
          }
          varDefs.add(varDef);
        }
      } else if (field.getAnnotation(IsFailure.class) != null
              || field.getAnnotation(TestCaseId.class) != null) {
        // ignore for system def
        // TODO: Check type, raise exception if unusable
      } else {
        org.cornutum.tcases.VarSet varSet = new org.cornutum.tcases.VarSet(field.getName());
        Class<?> fieldClass = field.getType();
        // recursion, TODO: make sure not circular
        if (fieldClass.isEnum()) {
          throw new IllegalStateException("Cannot use Enum as VarSet. Hint: mark the enum field as @Var?");
        }
        for (Field nestedField: fieldClass.getDeclaredFields()) {
          for (IVarDef varDef : createVarDefs(nestedField)) {
            varSet.addMember(varDef);
          }
        }
        varDefs.add(varSet);
      }
      return varDefs;
    }

    private static VarValueDef.Type typeOf(Value varValue) {
      if (varValue.type() == TestCase.Type.FAILURE) {
        return VarValueDef.Type.FAILURE;
      }
      return VarValueDef.Type.VALID;
    }
  }

