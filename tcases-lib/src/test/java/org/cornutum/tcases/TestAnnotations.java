//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Map;

/**
 * Runs tests for output annotations.
 *
 */
public class TestAnnotations
  {
  @Test
  public void runWhenOutputAnnotations()
    {
    // Given...
    SystemInputDef inputDef = getSystemInputDefAnnotated();
    
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);
    
    // Then...
    verifyAnnotations( testDef);
    }

  /**
   * Returns the set of annotation bindings for the given Annotated instance.
   */
  private void verifyAnnotations( SystemTestDef testDef)
    {
    Collection<Entry<String,String>> sysAnnotations =
      expectedAnnotations
      ( new String[]{ "AS0", "VS0"},
        new String[]{ "AS1", "VS1"});
    assertThat( "System annotations", getAnnotations( testDef), containsMembers( sysAnnotations));
    
    FunctionTestDef f1 = testDef.getFunctionTestDef( "F1");
    assertThat( "F1 defined", f1 != null, is( true));

    Collection<Entry<String,String>> f1Annotations =
      expectedAnnotations
      ( new String[]{ "AF1", "VF1"},
        new String[]{ "AS0", "VF0"},
        new String[]{ "AS1", "VS1"});
    assertThat( "F1, annotations", getAnnotations( f1), containsMembers( f1Annotations));

    TestCase f1_tc0 = f1.getTestCase(0);
    assertThat( "F1, test=0 defined", f1_tc0 != null, is( true));
    assertThat( "F1, test=0 annotations", getAnnotations( f1_tc0), containsMembers( f1Annotations));

    VarBinding v1 = f1_tc0.getVarBinding( "Var-1");
    assertThat( "Var-1 defined ", v1 != null, is( true));
    Collection<Entry<String,String>> v1Annotations =
      expectedAnnotations
      ( new String[]{ "AL1", "VL1"},
        new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VV1"},
        new String[]{ "AV0", "VL0"});
    assertThat( "Var-1 annotations", getAnnotations( v1), containsMembers( v1Annotations));

    VarBinding v2 = f1_tc0.getVarBinding( "Var-2");
    assertThat( "Var-2 defined ", v2 != null, is( true));
    Collection<Entry<String,String>> v2Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VT1"});
    assertThat( "Var-2 annotations", getAnnotations( v2), containsMembers( v2Annotations));

    VarBinding v3 = f1_tc0.getVarBinding( "Var-3");
    assertThat( "Var-3 defined ", v3 != null, is( true));
    Collection<Entry<String,String>> v3Annotations = expectedAnnotations();
    assertThat( "Var-3 annotations", getAnnotations( v3), containsMembers( v3Annotations));
    
    TestCase f1_tc1 = f1.getTestCase(1);
    assertThat( "F1, test=1 defined", f1_tc1 != null, is( true));
    Collection<Entry<String,String>> f1tc1Annotations = expectedAnnotations( f1Annotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "property-1-2"});
    assertThat( "F1, test=1 annotations", getAnnotations( f1_tc1), containsMembers( f1tc1Annotations));

    v1 = f1_tc1.getVarBinding( "Var-1");
    assertThat( "Var-1 defined ", v1 != null, is( true));
    v1Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VV1"},
        new String[]{ "AV0", "VV0"});
    assertThat( "Var-1 annotations", getAnnotations( v1), containsMembers( v1Annotations));

    v2 = f1_tc1.getVarBinding( "Var-2");
    assertThat( "Var-2 defined ", v2 != null, is( true));
    v2Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VT1"});
    assertThat( "Var-2 annotations", getAnnotations( v2), containsMembers( v2Annotations));

    v3 = f1_tc1.getVarBinding( "Var-3");
    assertThat( "Var-3 defined ", v3 != null, is( true));
    v3Annotations = expectedAnnotations();
    assertThat( "Var-3 annotations", getAnnotations( v3), containsMembers( v3Annotations));
    
    FunctionTestDef f2 = testDef.getFunctionTestDef( "F2");
    assertThat( "F2 defined", f2 != null, is( true));
    Collection<Entry<String,String>> f2Annotations = expectedAnnotations( sysAnnotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "functionProperties"});
    assertThat( "F2 annotations", getAnnotations( f2), containsMembers( f2Annotations));

    TestCase f2_tc0 = f2.getTestCase(0);
    assertThat( "F2, test=0 defined", f2_tc0 != null, is( true));
    Collection<Entry<String,String>> f2tc0Annotations = expectedAnnotations( sysAnnotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "Alpha,Bravo,charlie,Delta,easy"});
    assertThat( "F2, test=0 annotations", getAnnotations( f2_tc0), containsMembers( f2tc0Annotations));

    v1 = f2_tc0.getVarBinding( "VarSet-3.Var-1");
    assertThat( "VarSet-3.Var-1 defined ", v1 != null, is( true));
    v1Annotations =
      expectedAnnotations
      ( new String[]{ "AT3", "VT3"},
        new String[]{ "AT4", "VVS4"},
        new String[]{ "AVS3", "VVS3"});
    assertThat( "VarSet-3.Var-1 annotations", getAnnotations( v1), containsMembers( v1Annotations));

    v2 = f2_tc0.getVarBinding( "Var-2");
    assertThat( "Var-2 defined ", v2 != null, is( true));
    v2Annotations = expectedAnnotations();
    assertThat( "Var-2 annotations", getAnnotations( v2), containsMembers( v2Annotations));

    v2.setAnnotationList( "LA1", Arrays.asList( null, 1, 2, null, 3));
    assertThat( "List annotation", v2.getAnnotationList( "LA1"), listsMembers( Arrays.asList( null, "1", "2", null, "3")));

    v2.setAnnotationList( "LA2", null);
    assertThat( "Null list annotation", v2.getAnnotationList( "LA2"), is( nullValue()));
    assertThat( "Undefined list annotation", v2.getAnnotationList( "LA3"), is( nullValue()));
    }

  /**
   * Returns the set of annotation bindings for the given Annotated instance.
   */
  private Collection<Entry<String,String>> getAnnotations( Annotated annotated)
    {
    Map<String,String> annotations = new HashMap<String,String>();
    for( Iterator<String> names = annotated.getAnnotations(); names.hasNext(); )
      {
      String name = names.next();
      String value = annotated.getAnnotation( name);
      annotations.put( name, value);
      }

    return annotations.entrySet();
    }

  /**
   * Returns the given set of expected annotation bindings.
   */
  private Collection<Entry<String,String>> expectedAnnotations( String[]... bindings)
    {
    return expectedAnnotations( new HashMap<String,String>(), bindings);
    }

  /**
   * Returns the given set of expected annotation bindings.
   */
  private Collection<Entry<String,String>> expectedAnnotations( Collection<Entry<String,String>> bindings, String[]... moreBindings)
    {
    Map<String,String> annotations = new HashMap<String,String>();
    bindings.forEach( binding -> annotations.put( binding.getKey(), binding.getValue()));

    return expectedAnnotations( annotations, moreBindings);
    }

  /**
   * Returns the given set of expected annotation bindings.
   */
  private Collection<Entry<String,String>> expectedAnnotations( Map<String,String> annotations, String[]... bindings)
    {
    for( int i = 0; i < bindings.length; i++)
      {
      String name = bindings[i][0];
      String value = bindings[i][1];
      annotations.put( name, value);
      }

    return annotations.entrySet();
    }

  private SystemInputDef getSystemInputDefAnnotated()
    {
    return
      SystemInputDefBuilder.with( "S")
      .has( "AS0", "VS0")
      .has( "AS1", "VS1")

      .functions(
        FunctionInputDefBuilder.with( "F1")
        .has( "AS0", "VF0")
        .has( "AF1", "VF1")
        .vars(
          "T1",

          VarDefBuilder.with( "Var-1")
          .has( "AT0", "VT0")
          .has( "AV0", "VV0")
          .has( "AT1", "VV1")
          .values(
            VarValueDefBuilder.with( "Value-1-1")
            .has( "AV0", "VL0")
            .has( "AL1", "VL1")
            .build(),
            VarValueDefBuilder.with( "Value-1-2")
            .properties( "property-1-2")
            .build())
          .build(),

          VarDefBuilder.with( "Var-2")
          .has( "AT0", "VT0")
          .has( "AT1", "VT1")
          .values(
            VarValueDefBuilder.with( "Value-2-1")
            .build())
          .build())
        
        .vars(
          "T2",

          VarDefBuilder.with( "Var-3")
          .values(
            VarValueDefBuilder.with( "Value-3-1")
            .build())
          .build())
        .build(),

        FunctionInputDefBuilder.with( "F2")
        // Special case: allowed but should not override TestCase "properties" annotations added automatically
        .has( "properties", "functionProperties")
        
        .vars(
          VarSetBuilder.with( "VarSet-3")
          .has( "AT3", "VT3")
          .has( "AVS3", "VVS3")
          .has( "AT4", "VVS4")
          .members(
            VarDefBuilder.with( "Var-1")
            .values(
              VarValueDefBuilder.with( "Value-1-1")
              .properties( "charlie", "easy")
              .build())
            .build())
          .build(),

          VarDefBuilder.with( "Var-2")
          .values(
            VarValueDefBuilder.with( "Value-2-1")
            .properties( "easy", "Bravo", "Delta", "Alpha")
            .build())
          .build())
        .build())
      
      .build();               
    }
  }
