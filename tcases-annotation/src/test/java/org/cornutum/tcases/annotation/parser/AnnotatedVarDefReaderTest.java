package org.cornutum.tcases.annotation.parser;

import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarSet;
import org.cornutum.tcases.annotation.*;
import org.cornutum.tcases.annotation.generator.OutputAnnotationContainer;
import org.junit.Test;

import static org.cornutum.tcases.annotation.parser.AnnotatedVarDefReader.createVarDef;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

public class AnnotatedVarDefReaderTest {

    @Test
    public void createVarDefs() throws Exception {
        Class<Samples> samplesClass = Samples.class;
        assertNull(createVarDef(samplesClass.getField("testCaseId")));
        assertNull(createVarDef(samplesClass.getField("isFailure")));
        assertNull(createVarDef(samplesClass.getField("having")));

        assertThat(createVarDef(samplesClass.getField("stringValue")), instanceOf(VarDef.class));
        assertThat(createVarDef(samplesClass.getField("enumValue")), instanceOf(VarDef.class));
        // TODO check annotations/conditions

        assertThat(createVarDef(samplesClass.getField("innerSamples")), instanceOf(VarSet.class));
        // TODO check annotations/conditions
    }

    private static class Samples {
        @TestCaseId // presence is optional
        public int testCaseId;

        @IsFailure // presence is optional, will be filled based on VarDefs
        public boolean isFailure;

        @OutputAnnotations // presence is optional, will be filled based on Output annotations
        public OutputAnnotationContainer having;

        @Var(values = @Value("foo"))
        public String stringValue;

        @Var
        public CardinalityZeroToN enumValue;

        public InnerSamples innerSamples;

        public enum CardinalityZeroToN {
            NONE,
            ONE,
            MANY;
        }

        private static class InnerSamples {

            public InnerSamples2 innerSamples2;

            private static class InnerSamples2 {

            }
        }
    }
}