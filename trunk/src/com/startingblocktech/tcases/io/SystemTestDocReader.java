//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.io;

import com.startingblocktech.tcases.*;
import static com.startingblocktech.tcases.DefUtils.*;
import static com.startingblocktech.tcases.io.SystemTestDoc.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * An {@link ISystemTestSource} that reads from an XML document.
 *
 * @version $Revision$, $Date$
 */
public class SystemTestDocReader extends DefaultHandler implements ISystemTestSource
  {
  /**
   * The base class for element handlers used by this parser.
   *
   * @version $Revision$, $Date$
   */
  protected class ElementHandler extends DefaultHandler
    {
    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is not defined.
     */
    public String requireAttribute( Attributes attributes, String attributeName, String attributeValue) throws SAXException
      {
      if( attributeValue == null)
        {
        throw new SAXParseException( "No \"" + attributeName + "\" attribute specified", getDocumentLocator()); 
        }

      return attributeValue;
      }

    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given integer attribute or null if undefined. Throws a SAXException if the attribute is invalid.
     */
    public Integer getInteger( Attributes attributes, String attributeName) throws SAXException
      {
      return toInteger( attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given integer attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public Integer requireInteger( Attributes attributes, String attributeName) throws SAXException
      {
      return toInteger( attributeName, requireAttribute( attributes, attributeName));
      }

    /**
     * Returns the given attribute value as an integer. Throws a SAXException if the attribute is not a valid integer.
     */
    public Integer toInteger( String attributeName, String attributeValue) throws SAXException
      {
      Integer integer = null;
      String value = StringUtils.trimToNull( attributeValue);

      if( value != null)
        {
        try
          {
          integer = Integer.valueOf( value);
          if( integer < 0)
            {
            throw new IllegalArgumentException( "Invalid value=" + integer + ": must be non-negative");
            }
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute", getDocumentLocator(), e); 
          }
        }
      
      return integer;
      }

    /**
     * Returns the value of the given identifier attribute or null if undefined. Throws a SAXException if the attribute is invalid.
     */
    public String getIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdentifier( attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdentifier( attributeName, requireAttribute( attributes, attributeName));
      }

    /**
     * Returns the given attribute value as an identifier. Throws a SAXException if the attribute is not a valid identifier.
     */
    public String toIdentifier( String attributeName, String attributeValue) throws SAXException
      {
      String id = StringUtils.trimToNull( attributeValue);

      if( id != null)
        {
        try
          {
          assertIdentifier( id);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute", getDocumentLocator(), e); 
          }
        }

      return id;
      }

    /**
     * Returns the value of the given identifier path or null if undefined. Throws a SAXException if the attribute is invalid.
     */
    public String getIdPath( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdPath( attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier path. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdPath( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdPath( attributeName, requireAttribute( attributes, attributeName));
      }

    /**
     * Returns the given attribute value as an identifier path. Throws a SAXException if the attribute is not a valid identifier path.
     */
    public String toIdPath( String attributeName, String attributeValue) throws SAXException
      {
      String id = StringUtils.trimToNull( attributeValue);

      if( id != null)
        {
        try
          {
          assertPath( id);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute", getDocumentLocator(), e); 
          }
        }

      return id;
      }
      
    /**
     * Returns the value of the given attribute. Returns null if the attribute value is undefined or empty.
     */
    public String getAttribute( Attributes attributes, String attributeName)
      {
      String value = attributes.getValue( attributeName);
      return StringUtils.isBlank( value)? null : value;
      }

    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return false;
      }
      
    /**
     * Changes the parent ElementHandler.
     */
    public void setParent( ElementHandler parent)
      {
      parent_ = parent;
      }

    /**
     * Returns the parent ElementHandler.
     */
    public ElementHandler getParent()
      {
      return parent_;
      }

    private ElementHandler parent_;
    }
  
  /**
   * Handles TestCases elements.
   *
   * @version $Revision$, $Date$
   */
  protected class TestCasesHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return FUNCTION_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setSystemTestDef( new SystemTestDef( requireIdentifier( attributes, SYSTEM_ATR)));
      }

    /**
     * Changes the {@link SystemTestDef} represented by this element.
     */
    private void setSystemTestDef( SystemTestDef inputDef)
      {
      systemTestDef_ = inputDef;
      }

    /**
     * Returns the {@link SystemTestDef} represented by this element.
     */
    public SystemTestDef getSystemTestDef()
      {
      return systemTestDef_;
      }
    }
  
  /**
   * Handles Function elements.
   *
   * @version $Revision$, $Date$
   */
  protected class FunctionHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return TESTCASE_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setFunctionTestDef( new FunctionTestDef( requireIdentifier( attributes, NAME_ATR)));
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      FunctionTestDef functionTestDef = getFunctionTestDef();
      TestCasesHandler parent = (TestCasesHandler) getParent();

      try
        {
        parent.getSystemTestDef().addFunctionTestDef( functionTestDef);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add definition for " + functionTestDef, getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link FunctionTestDef} represented by this element.
     */
    private void setFunctionTestDef( FunctionTestDef functionTestDef)
      {
      functionTestDef_ = functionTestDef;
      }

    /**
     * Returns the {@link FunctionTestDef} represented by this element.
     */
    public FunctionTestDef getFunctionTestDef()
      {
      return functionTestDef_;
      }

    private FunctionTestDef functionTestDef_;
    }
  
  /**
   * Handles TestCase elements.
   *
   * @version $Revision$, $Date$
   */
  protected class TestCaseHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return INPUT_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setTestCase( new TestCase( requireInteger( attributes, ID_ATR)));

      String failureAtr = StringUtils.trimToNull( getAttribute( attributes, FAILURE_ATR));
      boolean failure = failureAtr != null && BooleanUtils.toBoolean( failureAtr);
      setFailure( failure);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      TestCase testCase = getTestCase();
      if( isFailure() && (testCase.getType() == TestCase.Type.SUCCESS))
        {
        throw new SAXParseException( "No failure input specified for " + testCase, getDocumentLocator()); 
        }
      
      FunctionHandler parent = (FunctionHandler) getParent();
      try
        {
        parent.getFunctionTestDef().addTestCase( testCase);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add definition for " + testCase, getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link TestCase} represented by this element.
     */
    private void setTestCase( TestCase testCase)
      {
      testCase_ = testCase;
      }

    /**
     * Returns the {@link TestCase} represented by this element.
     */
    public TestCase getTestCase()
      {
      return testCase_;
      }

    /**
     * Changes if this is a failure test case.
     */
    public void setFailure( boolean failure)
      {
      failure_ = failure;
      }

    /**
     * Returns if this is a failure test case.
     */
    public boolean isFailure()
      {
      return failure_;
      }

    private TestCase testCase_;
    private boolean failure_;
    }
  
  /**
   * Handles Input elements.
   *
   * @version $Revision$, $Date$
   */
  protected class InputHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return VAR_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      String type = getIdentifier( attributes, TYPE_ATR);
      setType( type == null? IVarDef.ARG : type);
      }

    /**
     * Changes the input variable type represented by this element.
     */
    private void setType( String type)
      {
      type_ = type;
      }

    /**
     * Returns the input variable type represented by this element.
     */
    public String getType()
      {
      return type_;
      }

    /**
     * Returns the TestCaseHandler for this element.
     */
    public TestCaseHandler getTestCaseHandler()
      {
      return (TestCaseHandler) getParent();
      }

    private String type_;
    }
  
  /**
   * Handles Var elements.
   *
   * @version $Revision$, $Date$
   */
  protected class VarHandler extends ElementHandler
    {    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      InputHandler parent = (InputHandler) getParent();

      VarBinding binding =
        new VarBinding
        ( requireIdPath( attributes, NAME_ATR),
          parent.getType(),
          requireAttribute( attributes, VALUE_ATR));

      TestCaseHandler testCaseHandler = getTestCaseHandler();
      TestCase testCase = testCaseHandler.getTestCase();
      
      String failureAtr = StringUtils.trimToNull( getAttribute( attributes, FAILURE_ATR));
      boolean failure = failureAtr != null && BooleanUtils.toBoolean( failureAtr);
      if( failure && !testCaseHandler.isFailure())
        {
        throw
          new SAXParseException
          ( "Unexpected failure value=" + binding.getValue()
            + " for success test case " + testCase.getId(),
            getDocumentLocator()); 
        }
      
      binding.setValueValid( !failure);
      testCase.addVarBinding( binding);
      }

    /**
     * Returns the TestCaseHandler for this element.
     */
    public TestCaseHandler getTestCaseHandler()
      {
      return (TestCaseHandler) getParent().getParent();
      }
    }
  
  /**
   * Creates a new SystemTestDocReader object.
   */
  public SystemTestDocReader()
    {
    this( System.in);
    }
  
  /**
   * Creates a new SystemTestDocReader object.
   */
  public SystemTestDocReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemTestDef} instance.
   */
  public SystemTestDef getSystemTestDef()
    {
    SAXParser parser;
    try
      {
      parser = SAXParserFactory.newInstance().newSAXParser();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create SAXParser", e);
      }

    try
      {
      parser.parse( getInputStream(), this);
      }
    catch( SAXParseException spe)
      {
      throw
        new RuntimeException
        ( "Error in document at line=" + spe.getLineNumber(),
          spe);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read SystemTestDef", e);
      }

    return systemTestDef_;
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ = stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  /**
   * Pushes the given ElementHandler onto the top of the parse stack.
   */
  protected void pushElementHandler( ElementHandler handler)
    {
    elementHandlers_.add( 0, handler);
    }

  /**
   * Removes an ElementHandler from the top of the parse stack.
   */
  protected ElementHandler popElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.remove( 0) : null;
    }

  /**
   * Returns the ElementHandler at the top of the parse stack.
   */
  protected ElementHandler getCurrentElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.get( 0) : null;
    }
    
  public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
    {
    ElementHandler handler =
      qName.equals( FUNCTION_TAG)?    (ElementHandler) new FunctionHandler() :
      qName.equals( INPUT_TAG)?       (ElementHandler) new InputHandler() :
      qName.equals( TESTCASES_TAG)?   (ElementHandler) new TestCasesHandler() :
      qName.equals( TESTCASE_TAG)?    (ElementHandler) new TestCaseHandler() :
      qName.equals( VAR_TAG)?         (ElementHandler) new VarHandler() :
      null;

    if( handler == null)
      {
      throw new SAXParseException( "Unknown element: " + qName, getDocumentLocator()); 
      }

    ElementHandler parentHandler = getCurrentElementHandler();
    if( !(parentHandler == null
          ? TESTCASES_TAG.equals( qName)
          : parentHandler.isMember( qName)))
      {
      throw new SAXParseException( "The " + qName + " element is not allowed at this location", getDocumentLocator()); 
      }

    handler.setParent( parentHandler);
    pushElementHandler( handler);
    handler.startElement( uri, localName, qName, attributes);
    }

  public void endElement( String uri, String localName, String qName) throws SAXException
    {
    ElementHandler handler = getCurrentElementHandler();
    if( handler != null)
      {
      handler.endElement( uri, localName, qName);
      popElementHandler(); 
      }
    }

  public void setDocumentLocator( Locator locator)
    {
    locator_ = locator;
    }

  public Locator getDocumentLocator()
    {
    return locator_;
    }
    
  public void warning( SAXParseException e) throws SAXException
    {
    }

  public void error( SAXParseException e) throws SAXException
    {
    throw e;
    }

  private InputStream           stream_;
  private Locator               locator_;
  private List<ElementHandler>  elementHandlers_  = new ArrayList<ElementHandler>();
  private SystemTestDef         systemTestDef_;
  }
