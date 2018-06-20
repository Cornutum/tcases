package org.cornutum.tcases.annotation.reader;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.PropertySet;
import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.VarValueDef;
import org.cornutum.tcases.annotation.Has;
import org.cornutum.tcases.annotation.Value;
import org.cornutum.tcases.annotation.Var;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.cornutum.tcases.annotation.reader.VarValueDefReader.readVarValueDefs;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.*;

public class VarValueDefReaderTest {

    @Test
    public void testReadVarValueDefsString() throws Exception {
        Class<StringSamples> fieldSamplesClass = StringSamples.class;

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidNoVar"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidNoValue"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        assertThat(readVarValueDefs(fieldSamplesClass.getField("aStringWithVar1Value")),
                hasSize(1));

        List<VarValueDef> aBooleanOnceFailure = readVarValueDefs(fieldSamplesClass.getField("aStringOnceFailure"));
        assertThat(aBooleanOnceFailure, hasSize(1));
        assertThat(aBooleanOnceFailure.get(0).getType(), equalTo(VarValueDef.Type.FAILURE));

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidDuplicate"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        List<VarValueDef> aStringWithVar2Value
                = readVarValueDefs(fieldSamplesClass.getField("aStringWithVar2Value"));
        assertThat(aStringWithVar2Value, hasSize(2));
        VarValueDef fooValue = aStringWithVar2Value.get(0);
        VarValueDef barValue = aStringWithVar2Value.get(1);

        assertThat(fooValue.getType(), equalTo(VarValueDef.Type.ONCE));
        assertThat(barValue.getType(), equalTo(VarValueDef.Type.FAILURE));

        assertTrue(fooValue.getProperties().contains("fooProp"));
        assertFalse(fooValue.getProperties().contains("barProp"));
        assertFalse(barValue.getProperties().contains("fooProp"));
        assertTrue(barValue.getProperties().contains("barProp"));

        List<String> fooAnnotations = IteratorUtils.toList(fooValue.getAnnotations());
        assertThat(fooAnnotations, equalTo(Arrays.asList("fooHasKey")));
        assertThat(fooValue.getAnnotation("fooHasKey"), equalTo("fooHasValue"));

        List<String> barAnnotations = IteratorUtils.toList(barValue.getAnnotations());
        assertThat(barAnnotations, equalTo(Arrays.asList("barHasKey")));
        assertThat(barValue.getAnnotation("barHasKey"), equalTo("barHasValue"));

        assertTrue(fooValue.getCondition().satisfied(new PropertySet("fooWhen")));
        assertFalse(fooValue.getCondition().satisfied(new PropertySet()));
        assertFalse(barValue.getCondition().satisfied(new PropertySet("fooWhen")));
        assertTrue(barValue.getCondition().satisfied(new PropertySet()));
    }

    private static class StringSamples {
        public String invalidNoVar;

        @Var
        public String invalidNoValue;

        @Var(values = @Value("foo"))
        public String aStringWithVar1Value;

        @Var(values = {
                @Value(value = "foo",
                        properties = {"fooProp"},
                        once = true,
                        when = "fooWhen",
                        having = @Has(name = "fooHasKey", value = "fooHasValue")),
                @Value(value = "bar",
                        properties = {"barProp"},
                        whenNot = "fooWhen",
                        type = TestCase.Type.FAILURE,
                        having = @Has(name = "barHasKey", value = "barHasValue"))
        })
        public String aStringWithVar2Value;

        @Var(values = {@Value(value = "fail", once = true, type = TestCase.Type.FAILURE)})
        public String aStringOnceFailure;

        @Var(values = {@Value("fail"), @Value("fail")})
        public String invalidDuplicate;
    }

    @Test
    public void testReadVarValueDefsBoolean() throws Exception {
        Class<BooleanFieldSamples> fieldSamplesClass = BooleanFieldSamples.class;
        assertThat(readVarValueDefs(fieldSamplesClass.getField("aBoolean")),
                hasSize(2));
        assertThat(readVarValueDefs(fieldSamplesClass.getField("aBooleanWithVar")),
                hasSize(2));
        assertThat(readVarValueDefs(fieldSamplesClass.getField("aBooleanWithVar1Value")),
                hasSize(2));

        List<VarValueDef> aBooleanOnceFailure = readVarValueDefs(fieldSamplesClass.getField("aBooleanOnceFailure"));
        assertThat(aBooleanOnceFailure, hasSize(2));
        assertThat(aBooleanOnceFailure.get(0).getType(), equalTo(VarValueDef.Type.FAILURE));

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidDuplicate"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidUnknown"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        List<VarValueDef> aBooleanWithVar2Value
                = readVarValueDefs(fieldSamplesClass.getField("aBooleanWithVar2Value"));
        assertThat(aBooleanWithVar2Value, hasSize(2));
        VarValueDef trueValue = aBooleanWithVar2Value.get(0);
        VarValueDef falseValue = aBooleanWithVar2Value.get(1);

        assertThat(trueValue.getType(), equalTo(VarValueDef.Type.ONCE));
        assertThat(falseValue.getType(), equalTo(VarValueDef.Type.FAILURE));

        assertTrue(trueValue.getProperties().contains("trueProp"));
        assertFalse(trueValue.getProperties().contains("falseProp"));
        assertFalse(falseValue.getProperties().contains("trueProp"));
        assertTrue(falseValue.getProperties().contains("falseProp"));

        List<String> trueAnnotations = IteratorUtils.toList(trueValue.getAnnotations());
        assertThat(trueAnnotations, equalTo(Arrays.asList("trueHasKey")));
        assertThat(trueValue.getAnnotation("trueHasKey"), equalTo("trueHasValue"));

        List<String> falseAnnotations = IteratorUtils.toList(falseValue.getAnnotations());
        assertThat(falseAnnotations, equalTo(Arrays.asList("falseHasKey")));
        assertThat(falseValue.getAnnotation("falseHasKey"), equalTo("falseHasValue"));

        assertTrue(trueValue.getCondition().satisfied(new PropertySet("trueWhen")));
        assertFalse(trueValue.getCondition().satisfied(new PropertySet()));
        assertFalse(falseValue.getCondition().satisfied(new PropertySet("trueWhen")));
        assertTrue(falseValue.getCondition().satisfied(new PropertySet()));
    }

    private static class BooleanFieldSamples {

        public Boolean aBoolean;

        @Var
        public Boolean aBooleanWithVar;

        @Var(values = @Value("true"))
        public Boolean aBooleanWithVar1Value;

        @Var(values = {
                @Value(value = "true",
                        properties = {"trueProp"},
                        once = true,
                        when = "trueWhen",
                        having = @Has(name = "trueHasKey", value = "trueHasValue")),
                @Value(value = "false",
                        properties = {"falseProp"},
                        whenNot = "trueWhen",
                        type = TestCase.Type.FAILURE,
                        having = @Has(name = "falseHasKey", value = "falseHasValue"))
        })
        public Boolean aBooleanWithVar2Value;

        @Var(values = {@Value(value = "true", once = true, type = TestCase.Type.FAILURE)})
        public Boolean aBooleanOnceFailure;

        @Var(values = {@Value("true"), @Value("true")})
        public Boolean invalidDuplicate;

        @Var(values = {@Value("unknown")})
        public Boolean invalidUnknown;
    }

    @Test
    public void testReadVarValueDefsEnum() throws Exception {
        Class<EnumFieldSamples> fieldSamplesClass = EnumFieldSamples.class;

        try {
            readVarValueDefs(fieldSamplesClass.getField("enum0Field"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        assertThat(readVarValueDefs(fieldSamplesClass.getField("enum1Field")),
                hasSize(1));
        assertThat(readVarValueDefs(fieldSamplesClass.getField("enum3Field")),
                hasSize(3));
        assertThat(readVarValueDefs(fieldSamplesClass.getField("enum3FieldVar")),
                hasSize(3));

        List<VarValueDef> aBooleanOnceFailure = readVarValueDefs(fieldSamplesClass.getField("aBooleanOnceFailure"));
        assertThat(aBooleanOnceFailure, hasSize(1));
        assertThat(aBooleanOnceFailure.get(0).getType(), equalTo(VarValueDef.Type.FAILURE));


        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidDuplicate"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
        }

        try {
            readVarValueDefs(fieldSamplesClass.getField("invalidUnknown"));
            fail("expected IllegalStateException");
        } catch (IllegalStateException e) {
            // pass
            assert e != null;
        }

        List<VarValueDef> aEnumWithVar2Value
                = readVarValueDefs(fieldSamplesClass.getField("enum3FieldVarFull"));
        assertThat(aEnumWithVar2Value, hasSize(3));
        VarValueDef a1Value = aEnumWithVar2Value.get(0);
        VarValueDef a2Value = aEnumWithVar2Value.get(1);
        VarValueDef a3Value = aEnumWithVar2Value.get(2);

        assertThat(a1Value.getType(), equalTo(VarValueDef.Type.ONCE));
        assertThat(a2Value.getType(), equalTo(VarValueDef.Type.FAILURE));

        assertTrue(a1Value.getProperties().contains("a1Prop"));
        assertFalse(a1Value.getProperties().contains("a2Prop"));
        assertFalse(a2Value.getProperties().contains("a1Prop"));
        assertTrue(a2Value.getProperties().contains("a2Prop"));
        assertFalse(a3Value.getProperties().contains("a2Prop"));
        assertFalse(a3Value.getProperties().contains("a1Prop"));

        List<String> a1Annotations = IteratorUtils.toList(a1Value.getAnnotations());
        assertThat(a1Annotations, equalTo(Arrays.asList("a1HasKey")));
        assertThat(a1Value.getAnnotation("a1HasKey"), equalTo("a1HasValue"));

        List<String> a2Annotations = IteratorUtils.toList(a2Value.getAnnotations());
        assertThat(a2Annotations, equalTo(Arrays.asList("a2HasKey")));
        assertThat(a2Value.getAnnotation("a2HasKey"), equalTo("a2HasValue"));

        assertTrue(a1Value.getCondition().satisfied(new PropertySet("aWhen")));
        assertFalse(a1Value.getCondition().satisfied(new PropertySet()));
        assertFalse(a2Value.getCondition().satisfied(new PropertySet("aWhen")));
        assertTrue(a2Value.getCondition().satisfied(new PropertySet()));
        assertTrue(a3Value.getCondition().satisfied(new PropertySet()));
        assertTrue(a3Value.getCondition().satisfied(new PropertySet("aWhen")));
    }

    private static class EnumFieldSamples {

        public Enum0Sample enum0Field;
        public Enum1Sample enum1Field;
        public Enum3Sample enum3Field;

        @Var
        public Enum3Sample enum3FieldVar;

        @Var(values = {
                @Value(value = "A1",
                        properties = {"a1Prop"},
                        once = true,
                        when = "aWhen",
                        having = @Has(name = "a1HasKey", value = "a1HasValue")),
                @Value(value = "A2",
                        properties = {"a2Prop"},
                        type = TestCase.Type.FAILURE,
                        whenNot = "aWhen",
                        having = @Has(name = "a2HasKey", value = "a2HasValue"))}
        )
        public Enum3Sample enum3FieldVarFull;

        public enum Enum0Sample {
        }

        public enum Enum1Sample {
            A1
        }

        public enum Enum3Sample {
            A1, A2, A3
        }

        @Var(values = {@Value(value = "A1", once = true, type = TestCase.Type.FAILURE)})
        public Enum1Sample aBooleanOnceFailure;

        @Var(values = {@Value("A1"), @Value("A1")})
        public Enum3Sample invalidDuplicate;

        @Var(values = {@Value("A4")})
        public Enum3Sample invalidUnknown;
    }
}