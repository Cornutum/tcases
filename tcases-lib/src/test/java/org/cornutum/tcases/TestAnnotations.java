//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.io.SystemInputResources;
import org.cornutum.tcases.io.SystemTestResources;
import static org.cornutum.tcases.util.Asserts.*;

import org.junit.Test;
import static org.junit.Assert.*;

import java.io.File;
import java.net.URL;
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
    assertSetEquals( "System annotations", sysAnnotations, getAnnotations( testDef));
    
    FunctionTestDef f1 = testDef.getFunctionTestDef( "F1");
    assertEquals( "F1 defined", true, f1 != null);

    Collection<Entry<String,String>> f1Annotations =
      expectedAnnotations
      ( new String[]{ "AF1", "VF1"},
        new String[]{ "AS0", "VF0"},
        new String[]{ "AS1", "VS1"});
    assertSetEquals( "F1, annotations", f1Annotations, getAnnotations( f1));

    TestCase f1_tc0 = f1.getTestCase(0);
    assertEquals( "F1, test=0 defined", true, f1_tc0 != null);
    assertSetEquals( "F1, test=0 annotations", f1Annotations, getAnnotations( f1_tc0));

    VarBinding v1 = f1_tc0.getVarBinding( "Var-1");
    assertEquals( "Var-1 defined ", true, v1 != null);
    Collection<Entry<String,String>> v1Annotations =
      expectedAnnotations
      ( new String[]{ "AL1", "VL1"},
        new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VV1"},
        new String[]{ "AV0", "VL0"});
    assertSetEquals( "Var-1 annotations", v1Annotations, getAnnotations( v1));

    VarBinding v2 = f1_tc0.getVarBinding( "Var-2");
    assertEquals( "Var-2 defined ", true, v2 != null);
    Collection<Entry<String,String>> v2Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VT1"});
    assertSetEquals( "Var-2 annotations", v2Annotations, getAnnotations( v2));

    VarBinding v3 = f1_tc0.getVarBinding( "Var-3");
    assertEquals( "Var-3 defined ", true, v3 != null);
    Collection<Entry<String,String>> v3Annotations = expectedAnnotations();
    assertSetEquals( "Var-3 annotations", v3Annotations, getAnnotations( v3));
    
    TestCase f1_tc1 = f1.getTestCase(1);
    assertEquals( "F1, test=1 defined", true, f1_tc1 != null);
    Collection<Entry<String,String>> f1tc1Annotations = expectedAnnotations( f1Annotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "property-1-2"});
    assertSetEquals( "F1, test=1 annotations", f1tc1Annotations, getAnnotations( f1_tc1));

    v1 = f1_tc1.getVarBinding( "Var-1");
    assertEquals( "Var-1 defined ", true, v1 != null);
    v1Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VV1"},
        new String[]{ "AV0", "VV0"});
    assertSetEquals( "Var-1 annotations", v1Annotations, getAnnotations( v1));

    v2 = f1_tc1.getVarBinding( "Var-2");
    assertEquals( "Var-2 defined ", true, v2 != null);
    v2Annotations =
      expectedAnnotations
      ( new String[]{ "AT0", "VT0"},
        new String[]{ "AT1", "VT1"});
    assertSetEquals( "Var-2 annotations", v2Annotations, getAnnotations( v2));

    v3 = f1_tc1.getVarBinding( "Var-3");
    assertEquals( "Var-3 defined ", true, v3 != null);
    v3Annotations = expectedAnnotations();
    assertSetEquals( "Var-3 annotations", v3Annotations, getAnnotations( v3));
    
    FunctionTestDef f2 = testDef.getFunctionTestDef( "F2");
    assertEquals( "F2 defined", true, f2 != null);
    Collection<Entry<String,String>> f2Annotations = expectedAnnotations( sysAnnotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "functionProperties"});
    assertSetEquals( "F2 annotations", f2Annotations, getAnnotations( f2));

    TestCase f2_tc0 = f2.getTestCase(0);
    assertEquals( "F2, test=0 defined", true, f2_tc0 != null);
    Collection<Entry<String,String>> f2tc0Annotations = expectedAnnotations( sysAnnotations, new String[]{ Annotated.TEST_CASE_PROPERTIES, "Alpha,Bravo,charlie,Delta,easy"});
    assertSetEquals( "F2, test=0 annotations", f2tc0Annotations, getAnnotations( f2_tc0));

    v1 = f2_tc0.getVarBinding( "VarSet-3.Var-1");
    assertEquals( "VarSet-3.Var-1 defined ", true, v1 != null);
    v1Annotations =
      expectedAnnotations
      ( new String[]{ "AT3", "VT3"},
        new String[]{ "AT4", "VVS4"},
        new String[]{ "AVS3", "VVS3"});
    assertSetEquals( "VarSet-3.Var-1 annotations", v1Annotations, getAnnotations( v1));

    v2 = f2_tc0.getVarBinding( "Var-2");
    assertEquals( "Var-2 defined ", true, v2 != null);
    v2Annotations = expectedAnnotations();
    assertSetEquals( "Var-2 annotations", v2Annotations, getAnnotations( v2));
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
