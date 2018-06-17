//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.*;

import java.lang.reflect.Field;
import java.util.ArrayList;
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
          for (Value varValue : varAnnotation.values()) {
            varDef.addValue(new VarValueDef(varValue.value(), varValue.failure() ? VarValueDef.Type.FAILURE : VarValueDef.Type.VALID));
          }
          varDefs.add(varDef);
        }
      } else {
        org.cornutum.tcases.VarSet varSet = new org.cornutum.tcases.VarSet(field.getName());
        Class<?> fieldClass = field.getType();
        // recursion, TODO: make sure not circular
        for (Field nestedField: fieldClass.getDeclaredFields()) {
          for (IVarDef varDef : createVarDefs(nestedField)) {
            varSet.addMember(varDef);
          }
        }
        varDefs.add(varSet);
      }
      return varDefs;
    }
  }

