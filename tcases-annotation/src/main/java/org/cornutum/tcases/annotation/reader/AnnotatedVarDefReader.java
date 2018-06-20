//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.reader;

import org.cornutum.tcases.IVarDef;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.annotation.*;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import static org.cornutum.tcases.annotation.reader.ConditionReader.getCondition;
import static org.cornutum.tcases.annotation.reader.VarValueDefReader.readVarValueDefs;

/**
 * Given a Java Bean classes annotated with Tcases annotations, created an IVarDef
 */
public class AnnotatedVarDefReader {

  private AnnotatedVarDefReader() {
  }

  /**
   * create VarSet of VarDef for a field depending on the annotations.
   */
  static IVarDef readVarDef(Field field) {
    IVarDef varDef = null;
    
    if (field.getAnnotation(Var.class) != null) {
      varDef = readVarDefFromVarField(field);
    } else {
      varDef = readVarSet(field);
    }
    return varDef;
  }

  /**
   * Create a VarSet for a field that has no @Var annotation, is not a Primitive or an enum.
   */
  private static org.cornutum.tcases.VarSet readVarSet(Field field) {
    org.cornutum.tcases.VarSet varSet = new org.cornutum.tcases.VarSet(field.getName());
    VarSet varSetAnnotation = field.getAnnotation(VarSet.class);
    if (varSetAnnotation != null) {
      varSet.setCondition(getCondition(varSetAnnotation.when(), varSetAnnotation.whenNot()));
      for (Has has : varSetAnnotation.having()) {
        varSet.setAnnotation(has.name(), has.value());
      }
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
        IVarDef varDef = readVarDef(nestedField);
        if (varDef != null) {
          varSet.addMember(varDef);
        }
      }
    }
    return varSet;
  }

  private static VarDef readVarDefFromVarField(Field field) {
    VarDef varDef = new VarDef(field.getName());
    Var varAnnotation = field.getAnnotation(Var.class);
    if (varAnnotation != null) {
      for (Has has : varAnnotation.having()) {
        varDef.setAnnotation(has.name(), has.value());
      }
      varDef.setCondition(getCondition(varAnnotation.when(), varAnnotation.whenNot()));
    }
    readVarValueDefs(field).forEach(varDef::addValue);
    return varDef;
  }

}
