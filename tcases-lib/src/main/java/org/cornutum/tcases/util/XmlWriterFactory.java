//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import java.io.OutputStream;
import java.io.Writer;
  
/**
 * Supports creation of an XML document stream.
 *
 */
public class XmlWriterFactory
  {
  /**
   * Creates a new XmlWriter object.
   */
  public XmlWriter forStream( OutputStream output)
    {
    return new XmlWriter( output);
    }

  /**
   * Creates a new XmlWriter object.
   */
  public XmlWriter forWriter( Writer writer)
    {
    return new XmlWriter( writer);
    }
  }
