//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.File;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.Map;

/**
 * Runs tests for reading and writing output annotations.
 *
 */
public class TestAnnotations
  {
  @Test
  public void writeOutputAnnotations()
    {
    // Given...
    SystemInputDef inputDef = inputResources_.read( "run-annotations-Input.xml");
    
    File inputDefFile = getResourceFile( "run-annotations-Output.xml");
    inputResources_.write( inputDef, inputDefFile);

    // When...
    SystemInputDef inputDefOut = inputResources_.read( inputDefFile);

    // Then...
    assertThat( "Writing system input definition", inputDefOut, matches( new SystemInputDefMatcher( inputDef)));
    }
  
  @Test
  public void runWhenOutputAnnotations()
    {
    // Given...
    SystemInputDef inputDef = inputResources_.read( "run-annotations-Input.xml");
    
    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);
    
    // Then...
    verifyAnnotations( testDef);

    // Given...
    File testDefFile = getResourceFile( "run-annotations-Test.xml");
    testResources_.write( testDef, testDefFile);

    // When...
    SystemTestDef testDefOut = testResources_.read( testDefFile);

    // Then...
    verifyAnnotations( testDefOut);
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

  /**
   * Return the file for the given resource.
   */
  private File getResourceFile( String resource)
    {
    URL classUrl = getClass().getResource( getClass().getSimpleName() + ".class");
    return new File( new File( classUrl.getFile()).getParent(), resource);
    }

  private SystemInputResources inputResources_ = new SystemInputResources( getClass());
  private SystemTestResources testResources_ = new SystemTestResources( getClass());
  }
