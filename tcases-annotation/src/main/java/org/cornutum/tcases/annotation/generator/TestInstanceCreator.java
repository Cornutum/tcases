package org.cornutum.tcases.annotation.generator;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.annotation.IsFailure;
import org.cornutum.tcases.annotation.TestCaseId;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

public class TestInstanceCreator {

  public static <T> T createDef(TestCase testCase, Class<T> functionDefClass) {
    T instance;
    try {
      // TODO: Consider util library for convenient reflection like objenesis
      instance = functionDefClass.getConstructor().newInstance();
      fillValues(0, instance, IteratorUtils.toList(testCase.getVarBindings()));
      fillSpecialValues(instance, testCase);
    } catch (NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
    return instance;
  }

  private static <T> void fillSpecialValues(T instance, TestCase testCase) {
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
    }
  }

  /**
   * recursively create and fill instance from varbinding values
   * @param prefixLength the initial varbinding key part to discard because of nesting depth
   */
  private static <T> void fillValues(int prefixLength, final T instance, Collection<VarBinding> varBindings) {
    Map<String, List<VarBinding>> unbound = new HashMap<>();
    varBindings.stream().forEach(binding -> {
      try {
        String name = binding.getVar().substring(prefixLength);
        int firstDotPos = name.indexOf('.');
        if (firstDotPos >= 0) {
          String mapKey = name.substring(0, firstDotPos);
          List<VarBinding> bindingList = unbound.getOrDefault(mapKey, new ArrayList<>());
          bindingList.add(binding);
          unbound.put(mapKey, bindingList);
        } else {
          Field f = instance.getClass().getDeclaredField(name);
          f.setAccessible(true);
          // TODO: Find better way to handle types, also primitive types
          if (f.getType() == Boolean.class) {
            f.set(instance, Boolean.valueOf(binding.getValue()));
          } else if (f.getType().isEnum()) {
              f.set(instance, Enum.valueOf((Class<Enum>)f.getType(), binding.getValue()));
          } else {
            f.set(instance, binding.getValue());
          }
        }
      } catch (NoSuchFieldException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    });
    unbound.entrySet().stream().forEach(entry -> {
      // TODO: Recurse properly
      try {
        Field f = instance.getClass().getDeclaredField(entry.getKey());
        f.setAccessible(true);
        Object fieldInstance = f.get(instance);
        if (fieldInstance == null) {
          fieldInstance = f.getType().getConstructor().newInstance();
          f.set(instance, fieldInstance);
        }
        fillValues(prefixLength + entry.getKey().length() + 1, fieldInstance, entry.getValue());
      } catch (NoSuchFieldException | NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    });
  }
}
