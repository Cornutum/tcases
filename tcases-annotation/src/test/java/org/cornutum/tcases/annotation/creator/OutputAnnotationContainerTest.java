//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.creator;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.FunctionTestDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.VarBinding;
import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

public class OutputAnnotationContainerTest {

    @Test
    public void testOutputContainer() {
        OutputAnnotationContainer container = new OutputAnnotationContainer();
        assertThat(IteratorUtils.toList(container.getTestCaseAnnotationKeys()), hasSize(0));
        assertThat(IteratorUtils.toList(container.getVarBindingAnnotationKeys()), hasSize(0));

        container.addTestCaseAnnotations(new SystemTestDef("foo1"));
        assertThat(IteratorUtils.toList(container.getTestCaseAnnotationKeys()), hasSize(0));
        assertThat(IteratorUtils.toList(container.getVarBindingAnnotationKeys()), hasSize(0));

        SystemTestDef systemTestDef = new SystemTestDef("foo1");
        systemTestDef.setAnnotation( "sysKey", "sysValue");
        FunctionTestDef fooFun = createFunctionTestDef("fooFun");
        systemTestDef.addFunctionTestDef(fooFun);
        FunctionTestDef barFun = createFunctionTestDef("barFun");
        systemTestDef.addFunctionTestDef(barFun);

        container.addTestCaseAnnotations(systemTestDef);
        // not adding functionAnnotations
        assertThat(IteratorUtils.toList(container.getTestCaseAnnotationKeys()),
                equalTo(Arrays.asList("sysKey")));
        assertThat(container.getTestCaseAnnotation("sysKey"),
                equalTo("sysValue"));
        assertThat(IteratorUtils.toList(container.getVarBindingAnnotationKeys()), hasSize(0));

        container.addTestCaseAnnotations(fooFun);
        container.addTestCaseAnnotations(barFun);
        // not adding functionAnnotations
        assertThat(IteratorUtils.toList(container.getTestCaseAnnotationKeys()),
                containsInAnyOrder("sysKey", "fooFunKey", "barFunKey"));
        assertThat(IteratorUtils.toList(container.getVarBindingAnnotationKeys()), hasSize(0));

        VarBinding varBinding1 = createVarBinding("v1");
        VarBinding varBinding2 = createVarBinding("v2");
        container.addVarBindingAnnotations("", varBinding1);
        container.addVarBindingAnnotations("nested", varBinding1);
        container.addVarBindingAnnotations("", varBinding2);
        assertThat(IteratorUtils.toList(container.getTestCaseAnnotationKeys()),
                containsInAnyOrder("sysKey", "fooFunKey", "barFunKey"));
        assertThat(IteratorUtils.toList(container.getVarBindingAnnotationKeys()),
                containsInAnyOrder("nested.v1AnnoKey", "v1AnnoKey", "v2AnnoKey"));
        assertThat(container.getVarBindingAnnotation("v2AnnoKey"),
                equalTo("v2AnnoValue"));
        assertThat(container.getVarBindingAnnotation("nested.v1AnnoKey"),
                equalTo("v1AnnoValue"));
   }

    private static FunctionTestDef createFunctionTestDef(String prefix) {
        FunctionTestDef functionTestDef = new FunctionTestDef(prefix);
        functionTestDef.setAnnotation(prefix + "Key", prefix + "Value");
        return functionTestDef;
    }

    private static VarBinding createVarBinding(String prefix) {
        VarBinding varBinding = new VarBinding(prefix, prefix + "Value");
        varBinding.setAnnotation(prefix + "AnnoKey", prefix + "AnnoValue");
        return varBinding;
    }
}