//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import java.io.InputStream;

/**
 * A {@link TransformFilter} that transforms a system test definition document
 * into Java source for JUnit tests.
 *
 * @version $Revision$, $Date$
 */
public class TestDefToJUnitFilter extends TransformFilter
  {
  /**
   * Creates a new TestDefToJUnitFilter object.
   */
  public TestDefToJUnitFilter()
    {
    super( getTransformDef());
    }

  /**
   * Returns the XSLT source stream for this filter.
   */
  private static InputStream getTransformDef()
    {
    String resource = "/testDef2Junit.xsl";
    InputStream resourceStream = TestDefToJUnitFilter.class.getResourceAsStream( resource);
    if( resourceStream == null)
      {
      throw new RuntimeException( "Can't locate resource=" + resource);
      }
    
    return resourceStream;
    }
  }
