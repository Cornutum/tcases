//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.parser;

import org.cornutum.tcases.IVarDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.annotation.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

/**
 * Given a Java Bean classes annotated with Tcases annotations, created an IVarDef
 */
public class AnnotatedVarDefReader {

  private AnnotatedVarDefReader() {
  }

  /**
   * create VarSet of VarDef for a field depending on the annotations.
   */
  static List<IVarDef> createVarDefs(Field field) {
    List<IVarDef> varDefs = new ArrayList<>();
    
    if (field.getAnnotation(Var.class) != null) {
      varDefs.add(getVarDefFromVarField(field));
    } else if (field.getAnnotation(IsFailure.class) == null
            && field.getAnnotation(OutputAnnotations.class) == null
            && field.getAnnotation(TestCaseId.class) == null) {
      varDefs.add(createVarSet(field));
    }
    return varDefs;
  }

  /**
   * Create a VarSet for a field that has no @Var annotation, is not a Primitive or an enum.
   */
  private static org.cornutum.tcases.VarSet createVarSet(Field field) {
    org.cornutum.tcases.VarSet varSet = new org.cornutum.tcases.VarSet(field.getName());
    VarSet varSetAnnotation = field.getAnnotation(VarSet.class);
    varSet.setCondition(ConditionReader.getCondition(varSetAnnotation.when(), varSetAnnotation.whenNot()));
    for (Has has : varSetAnnotation.having()) {
      varSet.setAnnotation(has.name(), has.value());
    }
    Class<?> fieldClass = field.getType();
    // recursion, TODO: make sure not circular
    if (fieldClass.isEnum()) {
      throw new IllegalStateException("Cannot use Enum as VarSet. Hint: mark the enum field as @Var?");
    }
    if (fieldClass.isPrimitive()) {
      throw new IllegalStateException("Cannot use Primitive as VarSet. Hint: mark the enum field as @Var?");
    }
    for (Field nestedField: fieldClass.getDeclaredFields()) {
      if (!Modifier.isStatic(nestedField.getModifiers())) {
        for (IVarDef varDef : createVarDefs(nestedField)) {
          varSet.addMember(varDef);
        }
      }
    }
    return varSet;
  }

  private static VarDef getVarDefFromVarField(Field field) {
    VarDef varDef = new VarDef(field.getName());
    Var varAnnotation = field.getAnnotation(Var.class);
    if (varAnnotation != null) {
      for (Has has : varAnnotation.having()) {
        varDef.setAnnotation(has.name(), has.value());
      }
      varDef.setCondition(ConditionReader.getCondition(varAnnotation.when(), varAnnotation.whenNot()));
    }
    VarValueDefReader.getVarValueDefs(field).forEach(varDef::addValue);
    return varDef;
  }

}
