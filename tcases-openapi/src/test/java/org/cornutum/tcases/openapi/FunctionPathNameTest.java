//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.InputModeller.functionPathName;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

public class FunctionPathNameTest
  {
  @Test
  public void whenSingleElement()
    {
    // Given...
    String pathName = "/someResource";
    
    // When...
    String functionPathName = functionPathName( pathName);
    
    // Then...
    assertThat( "Function path name", functionPathName, is( "someResource"));
    }
  
  @Test
  public void whenMultipleTemplates()
    {
    // Given...
    String pathName = "/{service}/{caseId}.{ext}";
    
    // When...
    String functionPathName = functionPathName( pathName);
    
    // Then...
    assertThat( "Function path name", functionPathName, is( "service-caseId-ext"));
    }
  
  @Test
  public void whenNonIdentifierChars()
    {
    // Given...
    String pathName = "/What /is {12?}/34{56?}+{78?}90=/?";
    
    // When...
    String functionPathName = functionPathName( pathName);
    
    // Then...
    assertThat( "Function path name", functionPathName, is( "What-is-12-34-56-78-90"));
    }
  }
