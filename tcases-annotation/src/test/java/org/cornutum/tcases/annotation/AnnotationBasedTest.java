package org.cornutum.tcases.annotation;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.annotation.sample1.Find;
import org.cornutum.tcases.generator.GeneratorSet;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

public class AnnotationBasedTest {


  public static final String SYSTEM = "findSystem";
  private GeneratorSet genDef;
  private SystemTestDef baseDef;
  private Tcases.Options options;

  @Before
  public void setUp() {
    genDef = GeneratorSet.basicGenerator();

    baseDef = null;
    options = new Tcases.Options();
  }

  @Test
  public void testFindSample() {
    SystemInputDef systemDef = AnnotationReader.createDef("findSystem", Find.class);
    assertNotNull(systemDef);
    assertThat(systemDef.getName(), equalTo(SYSTEM));
    List<FunctionInputDef> functionInputDefs = IteratorUtils.toList(systemDef.getFunctionInputDefs());
    assertThat(toString(systemDef), functionInputDefs.size(), equalTo(1));

    FunctionInputDef fun1Def = functionInputDefs.get(0);
    assertThat(fun1Def.getName(), equalTo("Find"));

    List<IVarDef> varDefs = IteratorUtils.toList(fun1Def.getVarDefs());
    assertThat(varDefs.size(), equalTo(3));

    assertNotNull(fun1Def.findVarPath("filename"));
    assertNotNull(fun1Def.findVarPath("pattern.size"));
    assertNotNull(fun1Def.findVarPath("file.exists"));

    /** generate testcases */

    SystemTestDef testDef = Tcases.getTests(systemDef, genDef, baseDef, options);
    assertThat(testDef.getName(), equalTo("findSystem"));

    List<FunctionTestDef> testDefsList = IteratorUtils.toList(testDef.getFunctionTestDefs());
    assertThat(toString(testDef), testDefsList.size(), equalTo(1));
    FunctionTestDef fun1TestDef = testDefsList.get(0);
    assertThat(fun1TestDef.getName(), equalTo(Find.class.getSimpleName()));

    List<TestCase> testCaseList = IteratorUtils.toList(fun1TestDef.getTestCases());
    assertThat(testCaseList.size(), equalTo(5));

    /** generate test instances */

    for (TestCase tcase : testCaseList) {
      Find f = TestInstanceCreator.createDef(tcase, Find.class);
      assertNotNull(f);
      System.out.println(f);
    }

  }

  private static String toString(SystemInputDef def) {
    return def.toString() + IteratorUtils.toList(def.getFunctionInputDefs());
  }

  private static String toString(SystemTestDef def) {
    return def.toString() + IteratorUtils.toList(def.getFunctionTestDefs());
  }

}