package org.cornutum.tcases.annotation.generator;

import org.cornutum.tcases.Annotated;

import java.util.HashMap;
import java.util.Map;

/**
 * To contain the collected output annotations from a systemTestDef.
 */
public class OutputAnnotationContainer {
    /**
     * testCase annotations include default Values from the system and function level, unless they have been overridden
     */
    public final Map<String, String> testCaseAnnotations = new HashMap<>();

    /**
     * variable binding annotations with the variable path
     */
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
