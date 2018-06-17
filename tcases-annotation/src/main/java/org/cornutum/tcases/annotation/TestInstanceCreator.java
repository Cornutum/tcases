package org.cornutum.tcases.annotation;

import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.VarBinding;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.*;

public class TestInstanceCreator {

  public static <T> T createDef(TestCase testCase, Class<T> functionDefClass) {
    T instance;
    try {
      // TODO: Consider util library for convenient reflection like objenesis
      instance = functionDefClass.getConstructor().newInstance();
      fillValues("", instance, testCase.getVarBindings());
    } catch (NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
    return instance;
  }

  private static <T> void fillValues(String prefix, final T instance, Iterator<VarBinding> varBindings) {
    Map<String, List<VarBinding>> unbound = new HashMap<>();
    varBindings.forEachRemaining(binding -> {
      try {
        String name = binding.getVar();
        int firstDotPos = name.indexOf('.');
        if (firstDotPos >= 0) {
          String mapKey = name.substring(0, firstDotPos);
          List<VarBinding> bindingList = unbound.getOrDefault(mapKey, new ArrayList<>());
          bindingList.add(binding);
          unbound.put(mapKey, bindingList);
        } else {
          Field f = instance.getClass().getDeclaredField(name);
          f.setAccessible(true);
          f.set(instance, binding.getValue());
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
        for (VarBinding binding : entry.getValue()) {
          // TODO: properly compute nested field name
          String fieldName = binding.getVar().substring(entry.getKey().length() + 1);
          Field nestedField = fieldInstance.getClass().getDeclaredField(fieldName);
          nestedField.setAccessible(true);
          // TODO: Find better way to handle types, also primitive types
          if (nestedField.getType() == Boolean.class) {
            nestedField.set(fieldInstance, Boolean.valueOf(binding.getValue()));
          } else {
            nestedField.set(fieldInstance, binding.getValue());
          }
        }
      } catch (NoSuchFieldException | NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    });
  }
}
