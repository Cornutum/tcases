package org.cornutum.tcases.annotation;

import org.apache.commons.collections4.IteratorUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.annotation.generator.TestInstanceCreator;
import org.cornutum.tcases.annotation.parser.AnnotatedFunctionDefReader;
import org.cornutum.tcases.annotation.sample1.FindFunction;
import org.cornutum.tcases.generator.GeneratorSet;
import org.cornutum.tcases.generator.TupleGenerator;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * Comprehensive Test using the sample1.FindFunction class as SystemTestDefinition/FunctionDefinition
 */
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


  /**
   * TODO: proper unit tests after code cleanup
   */
  @Test
  public void testSystemDefFromAnnotations() {
    SystemInputDef systemDef = AnnotatedFunctionDefReader.createSystemDef("findSystem", FindFunction.class);
    assertNotNull(systemDef);
    assertThat(systemDef.getName(), equalTo(SYSTEM));
    List<FunctionInputDef> functionInputDefs = IteratorUtils.toList(systemDef.getFunctionInputDefs());
    assertThat(toString(systemDef), functionInputDefs.size(), equalTo(1));

    FunctionInputDef fun1Def = functionInputDefs.get(0);
    assertThat(fun1Def.getName(), equalTo("Find"));

    List<IVarDef> varDefs = IteratorUtils.toList(fun1Def.getVarDefs());
    assertThat(varDefs.size(), equalTo(3));

    assertNotNull(fun1Def.findVarPath("filenameDefined"));
    assertNotNull(fun1Def.findVarPath("pattern.size"));
    assertNotNull(fun1Def.findVarPath("file.exists"));
  }

  /**
   * TODO: proper unit tests after code cleanup
   */
  @Test
  public void testTestDefFromAnnotations() {
    SystemInputDef systemDef = AnnotatedFunctionDefReader.createSystemDef("findSystem", FindFunction.class);

    /* generate testcases */

    SystemTestDef testDef = Tcases.getTests(systemDef, genDef, baseDef, options);
    assertThat(testDef.getName(), equalTo("findSystem"));

    List<FunctionTestDef> testDefsList = IteratorUtils.toList(testDef.getFunctionTestDefs());
    assertThat(toString(testDef), testDefsList.size(), equalTo(1));
    FunctionTestDef fun1TestDef = testDefsList.get(0);
    assertThat(fun1TestDef.getName(), equalTo("Find"));

    List<TestCase> testCaseList = IteratorUtils.toList(fun1TestDef.getTestCases());
    // check total number
    assertThat(testCaseList.size(), equalTo(10));
    // check failure number
    assertThat(testCaseList.stream().filter(testCase -> testCase.getType() == TestCase.Type.FAILURE).count(), equalTo(4L));
    // Check id
    for (int i = 0; i < testCaseList.size(); i++) {
      assertThat(testCaseList.get(i).getId(), equalTo(i));
    }
  }

  /**
   * TODO: proper unit tests after code cleanup
   */
  @Test
  public void testInstanceCreation() {
    SystemInputDef systemDef = AnnotatedFunctionDefReader.createSystemDef("findSystem", FindFunction.class);

    /* generate testcases */

    SystemTestDef testDef = Tcases.getTests(systemDef, genDef, baseDef, options);
    List<FunctionTestDef> testDefsList = IteratorUtils.toList(testDef.getFunctionTestDefs());
    FunctionTestDef fun1TestDef = testDefsList.get(0);
    List<TestCase> testCaseList = IteratorUtils.toList(fun1TestDef.getTestCases());

    /* generate test instances */
    List<FindFunction> findList = TestInstanceCreator.createDefs(testDef, "Find", FindFunction.class);

    assertThat(findList.size(), equalTo(testCaseList.size()));
    // check failure number
    assertThat(findList.stream().filter(testCase -> testCase.isFailure).count(), equalTo(4L));
    // Check id
    for (int i = 0; i < findList.size(); i++) {
      assertThat(findList.get(i).testCaseId, equalTo(i));
      System.out.println(findList.get(i));
    }
  }

  /**
   * TODO: proper unit tests after code cleanup
   */
  @Test
  public void testTestDefFromAnnotations2Tupel() {
    SystemInputDef systemDef = AnnotatedFunctionDefReader.createSystemDef("findSystem", FindFunction.class);

    /* generate testcases */
    genDef.addGenerator("Find", new TupleGenerator(2));
    SystemTestDef testDef = Tcases.getTests(systemDef, genDef, baseDef, options);
    assertThat(testDef.getName(), equalTo("findSystem"));

    List<FunctionTestDef> testDefsList = IteratorUtils.toList(testDef.getFunctionTestDefs());
    assertThat(toString(testDef), testDefsList.size(), equalTo(1));
    FunctionTestDef fun1TestDef = testDefsList.get(0);
    assertThat(fun1TestDef.getName(), equalTo("Find"));

    List<TestCase> testCaseList = IteratorUtils.toList(fun1TestDef.getTestCases());
    // check total number
    assertThat(testCaseList.size(), equalTo(24));
    // check failure number
    assertThat(testCaseList.stream().filter(testCase -> testCase.getType() == TestCase.Type.FAILURE).count(), equalTo(4L));
    // Check id
    for (int i = 0; i < testCaseList.size(); i++) {
      assertThat(testCaseList.get(i).getId(), equalTo(i));
    }
  }

  private static String toString(SystemInputDef def) {
    return def.toString() + IteratorUtils.toList(def.getFunctionInputDefs());
  }

  private static String toString(SystemTestDef def) {
    return def.toString() + IteratorUtils.toList(def.getFunctionTestDefs());
  }

}