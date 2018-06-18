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
import org.cornutum.tcases.annotation.VarSet;
import org.cornutum.tcases.conditions.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Gien a Java Bean classes annotated with Tcases annotations, created a SystemInputDef
 */
public class AnnotationReader
  {
    private AnnotationReader() {
    }

    public static SystemInputDef createDef(String systemName, Class<?>... functionDefClass) {
      SystemInputDef inputDef = new SystemInputDef(systemName);

      for (Class<?> annotatedClass: functionDefClass) {
        Function functionAnnotation = annotatedClass.getAnnotation(Function.class);
        String functionName = (functionAnnotation == null || StringUtils.isBlank(functionAnnotation.value())) ? annotatedClass.getSimpleName(): functionAnnotation.value();
        FunctionInputDef functionDef = new FunctionInputDef(functionName);
        if (functionAnnotation != null) {
          for (Has has : functionAnnotation.having()) {
            functionDef.setAnnotation(has.name(), has.value());
          }
        }
        inputDef.addFunctionInputDef(functionDef);
        for (Field field: annotatedClass.getDeclaredFields()) {
          if (!Modifier.isStatic(field.getModifiers())) {
            for (IVarDef varDef : createVarDefs(field)) {
              functionDef.addVarDef(varDef);
            }
          }
        }
      }

      return inputDef;
    }

    private static List<IVarDef> createVarDefs(Field field) {
      List<IVarDef> varDefs = new ArrayList<>();
      Var varAnnotation = field.getAnnotation(Var.class);

      if (varAnnotation != null) {
        VarDef varDef = new VarDef(field.getName());
        for (Has has : varAnnotation.having()) {
          varDef.setAnnotation(has.name(), has.value());
        }
        varDef.setCondition(getCondition(varAnnotation.when(), varAnnotation.whenNot()));
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
                  varDef.addValue(createVarValueDef(boolname, varValue));
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
                  varDef.addValue(createVarValueDef(enumField.getName(), varValue));
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
              varDef.addValue(createVarValueDef(varValue.value(), varValue));
            }
          } else {
            throw new IllegalStateException("@Var field must be enum or define values");
          }
        }
        varDefs.add(varDef);
      } else if (field.getAnnotation(IsFailure.class) != null
              || field.getAnnotation(OutputAnnotations.class) != null
              || field.getAnnotation(TestCaseId.class) != null) {
        // ignore for system def
        // TODO: Check type, raise exception if unusable
      } else {
        org.cornutum.tcases.VarSet varSet = new org.cornutum.tcases.VarSet(field.getName());
        VarSet varSetAnnotation = field.getAnnotation(VarSet.class);
        varSet.setCondition(getCondition(varSetAnnotation.when(), varSetAnnotation.whenNot()));
        for (Has has : varSetAnnotation.having()) {
          varSet.setAnnotation(has.name(), has.value());
        }
        Class<?> fieldClass = field.getType();
        // recursion, TODO: make sure not circular
        if (fieldClass.isEnum()) {
          throw new IllegalStateException("Cannot use Enum as VarSet. Hint: mark the enum field as @Var?");
        }
        for (Field nestedField: fieldClass.getDeclaredFields()) {
          if (!Modifier.isStatic(nestedField.getModifiers())) {
            for (IVarDef varDef : createVarDefs(nestedField)) {
              varSet.addMember(varDef);
            }
          }
        }
        varDefs.add(varSet);
      }
      return varDefs;
    }

    private static VarValueDef createVarValueDef(String name, Value varValue) {
      VarValueDef varValueDef = new VarValueDef(name, typeOf(varValue));
      for (Has has : varValue.having()) {
        varValueDef.setAnnotation(has.name(), has.value());
      }
      varValueDef.addProperties(varValue.properties());
      varValueDef.setCondition(getCondition(varValue.when(), varValue.whenNot()));
      return varValueDef;
    }

    private static VarValueDef.Type typeOf(Value varValue) {
      if (varValue.type() == TestCase.Type.FAILURE) {
        return VarValueDef.Type.FAILURE;
      }
      if (varValue.once()) {
        return VarValueDef.Type.ONCE;
      }
      return VarValueDef.Type.VALID;
    }

    public static ICondition getCondition(String[] when, String[] whenNot) {
      ICondition condition = null;

      if(when.length > 0)
      {
        condition = new ContainsAll(when);
      }

      if(whenNot.length > 0)
      {
        ICondition excludes = new Not().add(new ContainsAny(whenNot));

        condition =
                condition == null
                        ? excludes
                        : new AllOf().add( condition).add( excludes);
      }

      return condition;
    }
  }

