//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.creator;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.annotation.IsFailure;
import org.cornutum.tcases.annotation.OutputAnnotations;
import org.cornutum.tcases.annotation.TestCaseId;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

/**
 * Given the output of Tcases, creates an instance of a JavaBean
 * with fields filled according to VarBindings.
 */
public class TestInstanceCreator {

  public static <T> List<T> createDefs(SystemTestDef systemDef, String funName, Class<T> targetClass) {
    List<FunctionTestDef> testDefsList = IteratorUtils.toList(systemDef.getFunctionTestDefs());
    List<T> result = new ArrayList<>();
    for (FunctionTestDef funDef: testDefsList) {
      if (funDef.getName().equals(funName)) {
        funDef.getTestCases().forEachRemaining(testCase -> {
          OutputAnnotationContainer outputAnnotations = new OutputAnnotationContainer();
          // Intentionally allow overriding of values, according to Tcases specification
          outputAnnotations.addTestCaseAnnotations(systemDef);
          outputAnnotations.addTestCaseAnnotations(funDef);
          result.add(createDef(testCase, targetClass, outputAnnotations));
        });
      }
    }
    return result;
  }

  /**
   * @param outputAnnotations prefilled with System/Function Output values, to be added to T if annotation present
   */
  public static <T> T createDef(TestCase testCase,
                                Class<T> functionDefClass,
                                OutputAnnotationContainer outputAnnotations) {
    T instance;
    try {
      instance = functionDefClass.getConstructor().newInstance();
      outputAnnotations.addTestCaseAnnotations(testCase);
      fillValues(0, instance, IteratorUtils.toList(testCase.getVarBindings()), outputAnnotations);
      fillSpecialValues(instance, testCase, outputAnnotations);
    } catch (NoSuchMethodException
            | InvocationTargetException
            | InstantiationException
            | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
    return instance;
  }

  /**
   * Fill other fields with meta-information, if given annotation is present.
   */
  private static <T> void fillSpecialValues(T instance,
                                            TestCase testCase,
                                            OutputAnnotationContainer outputAnnotations) {
    for (Field f: instance.getClass().getDeclaredFields()) {
      if (f.getAnnotation(IsFailure.class) != null) {
        f.setAccessible(true);
        try {
          f.set(instance, testCase.getType() == TestCase.Type.FAILURE);
        } catch (IllegalAccessException e) {
          throw new RuntimeException(e);
        }
      }
      if (f.getAnnotation(TestCaseId.class) != null) {
        f.setAccessible(true);
        try {
          f.set(instance, testCase.getId());
        } catch (IllegalAccessException e) {
          throw new RuntimeException(e);
        }
      }
      if (f.getAnnotation(OutputAnnotations.class) != null) {
        f.setAccessible(true);
        try {
          f.set(instance, outputAnnotations);
        } catch (IllegalAccessException e) {
          throw new RuntimeException(e);
        }
      }
    }
  }

  /**
   * recursively create and fill instance from varbinding values
   * @param prefixLength the initial varbinding key part to discard because of nesting depth
   * @param outputAnnotations
   */
  @SuppressWarnings("unchecked")
  private static <T> void fillValues(int prefixLength, final T instance,
                                     Collection<VarBinding> varBindings,
                                     OutputAnnotationContainer outputAnnotations) {
    // each Varbinding is either for a single data field of this instance, or a nested field
    Map<String, List<VarBinding>> nestedFieldBindings = new HashMap<>();
    varBindings.forEach(binding -> {
      try {
        String name = binding.getVar().substring(prefixLength);
        outputAnnotations.addVarBindingAnnotations(name, binding);
        int firstDotPos = name.indexOf('.');
        if (firstDotPos >= 0) {
          String mapKey = name.substring(0, firstDotPos);
          List<VarBinding> bindingList = nestedFieldBindings.getOrDefault(mapKey, new ArrayList<>());
          bindingList.add(binding);
          nestedFieldBindings.put(mapKey, bindingList);
        } else {
          Field f = instance.getClass().getDeclaredField(name);
          f.setAccessible(true);
          // TODO: warn user when using "NA" somewhere :-(
          if (!"NA".equals(binding.getValue())) {
            // TODO: Find better way to handle types, also primitive types
            if (f.getType() == Boolean.class) {
              f.set(instance, Boolean.valueOf(binding.getValue()));
            } else if (f.getType().isEnum()) {
              f.set(instance, Enum.valueOf((Class<Enum>) f.getType(), binding.getValue()));
            } else {
              f.set(instance, binding.getValue());
            }
          }
        }
      } catch (NoSuchFieldException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    });
    nestedFieldBindings.forEach((key, value) -> {
      try {
        Field f = instance.getClass().getDeclaredField(key);
        f.setAccessible(true);
        Object fieldInstance = f.get(instance);
        if (fieldInstance == null) {
          fieldInstance = f.getType().getConstructor().newInstance();
          f.set(instance, fieldInstance);
        }
        fillValues(prefixLength + key.length() + 1, fieldInstance, value, outputAnnotations);
      } catch (NoSuchFieldException
              | NoSuchMethodException
              | InvocationTargetException
              | InstantiationException
              | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    });
  }

}
