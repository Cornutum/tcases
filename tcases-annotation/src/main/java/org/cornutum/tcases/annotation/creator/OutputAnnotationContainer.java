//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.creator;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.Annotated;
import org.cornutum.tcases.VarBinding;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * To contain the collected output annotations from a systemTestDef.
 */
public class OutputAnnotationContainer {
    /**
     * testCase annotations include default Values from the system and function level, unless they have been overridden
     */
    private final Map<String, String> testCaseAnnotations = new HashMap<>();

    /**
     * variable binding annotations with the variable path
     */
    private final Map<String, String> varBindingAnnotations = new HashMap<>();

    public void addTestCaseAnnotations(Annotated systemTestDef) {
        addAll(testCaseAnnotations, systemTestDef);
    }

    public void addVarBindingAnnotations(String path, VarBinding annotated) {
        annotated.getAnnotations().forEachRemaining(key -> {
            String prefixPath = StringUtils.isBlank(path) ? key : path + '.' + key;
            varBindingAnnotations.put(prefixPath, annotated.getAnnotation(key));
        });
    }

    public Iterator<String> getTestCaseAnnotationKeys() {
        return testCaseAnnotations.keySet().iterator();
    }

    public String getTestCaseAnnotation(String key) {
        return testCaseAnnotations.get(key);
    }

    public Iterator<String> getVarBindingAnnotationKeys() {
        return varBindingAnnotations.keySet().iterator();
    }

    public String getVarBindingAnnotation(String key) {
        return varBindingAnnotations.get(key);
    }

    private static void addAll(Map<String, String> map, Annotated annotated) {
        annotated.getAnnotations().forEachRemaining(key -> map.put(key, annotated.getAnnotation(key)));
    }

}
