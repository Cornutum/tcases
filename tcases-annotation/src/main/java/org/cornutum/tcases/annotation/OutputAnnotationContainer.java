package org.cornutum.tcases.annotation;

import org.cornutum.tcases.Annotated;

import java.util.HashMap;
import java.util.Map;

public class OutputAnnotationContainer {
    public final Map<String, String> testCaseAnnotations = new HashMap<>();
    public final Map<String, String> varBindingAnnotations = new HashMap<>();

    public void addTestCaseAnnotations(Annotated systemTestDef) {
        addAll(testCaseAnnotations, systemTestDef);
    }

    public void addVarBindingAnnotations(String path, Annotated annotated) {
        annotated.getAnnotations().forEachRemaining(key -> varBindingAnnotations.put(path + '.' + key, annotated.getAnnotation(key)));
    }

    private static void addAll(Map<String, String> map, Annotated annotated) {
        annotated.getAnnotations().forEachRemaining(key -> map.put(key, annotated.getAnnotation(key)));
    }
}
