package org.cornutum.tcases.annotation.reader;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.FunctionInputDef;
import org.cornutum.tcases.IVarDef;
import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.annotation.*;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.*;

public class AnnotatedFunctionDefReaderTest {


    @Test
    public void testCreateSytemDefEmpty() {
        SystemInputDef systemDef = AnnotatedFunctionDefReader.readSystemDef("Foo");
        assertThat(systemDef.getName(), equalTo("Foo"));

        systemDef = AnnotatedFunctionDefReader.readSystemDef("Baz", EmptyClass.class, EmptyClass2.class);
        assertThat(systemDef.getName(), equalTo("Baz"));
        assertThat(IteratorUtils.toList(systemDef.getFunctionInputDefs()), hasSize(2));
    }

    private static class EmptyClass {
    }

    @Function(" ")
    private static class EmptyClass2 {
    }


    @Test
    public void testReadFunctionInputDef() {

        FunctionInputDef funDef = AnnotatedFunctionDefReader.readFunctionInputDef(EmptyClass.class);
        assertNotNull(funDef);
        assertThat(funDef.getName(), equalTo("EmptyClass"));
        assertFalse(funDef.getVarDefs().hasNext());

        funDef = AnnotatedFunctionDefReader.readFunctionInputDef(EmptyClass2.class);
        assertNotNull(funDef);
        assertThat(funDef.getName(), equalTo("EmptyClass2"));
        assertFalse(funDef.getVarDefs().hasNext());

        try {
            AnnotatedFunctionDefReader.readFunctionInputDef(InvalidAnnotation.class);
            fail("Excpected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        funDef = AnnotatedFunctionDefReader.readFunctionInputDef(Sample1.class);
        assertThat(funDef.getName(), equalTo("altName"));
        assertThat(funDef.getAnnotation("foo"), equalTo("fooV"));
        assertThat(funDef.getAnnotation("bar"), equalTo("barV"));
        List<IVarDef> varDefs = IteratorUtils.toList(funDef.getVarDefs());
        assertThat(varDefs, hasSize(1));
        assertThat(varDefs.get(0).getName(), equalTo("varDef"));
    }

    private static class InvalidAnnotation {
        @Var
        public static Boolean staticField;
    }

    @Function(value = "altName", having = {
            @Has(name = "foo", value = "fooV"),
            @Has(name = "bar", value = "barV")
    })
    private static class Sample1 {
        private static Boolean ignoreStatic;

        @IsFailure
        private Boolean ignoreFailure;

        @OutputAnnotations
        private Boolean outputAnnotation;

        @TestCaseId
        private Boolean ignoretestCaseId;

        @Var
        private Boolean varDef;

    }

}