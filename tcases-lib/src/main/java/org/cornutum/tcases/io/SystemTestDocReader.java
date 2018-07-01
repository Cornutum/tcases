//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.DefUtils.*;
import static org.cornutum.tcases.io.SystemTestDoc.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * An {@link ISystemTestSource} that reads from an XML document.
 *
 */
public class SystemTestDocReader extends DefaultHandler implements ISystemTestSource
  {
  /**
   * The base class for element handlers used by this parser.
   *
   */
  protected abstract class ElementHandler extends DefaultHandler
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
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireNonBlankAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, StringUtils.trimToNull(getAttribute( attributes, attributeName)));
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
      return toInteger( attributeName, requireNonBlankAttribute( attributes, attributeName));
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
          try
            {
            integer = Integer.valueOf( value);
            }
          catch( Exception e)
            {
            throw new RuntimeException( "Invalid value=\"" + value + "\", must be a non-negative integer");
            }
          if( integer < 0)
            {
            throw new RuntimeException( "Invalid value=" + integer + ", must be non-negative");
            }
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
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
      return toIdentifier( attributeName, requireNonBlankAttribute( attributes, attributeName));
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
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
          }
        }

      return id;
      }

    /**
     * Returns the value of the given identifier path. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdPath( Attributes attributes, String attributeName) throws SAXException
      {
      String path = requireNonBlankAttribute( attributes, attributeName);

      try
        {
        assertPath( path);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
        }

      return path;
      }
      
    /**
     * Returns the value of the given attribute. Returns null if the attribute value is undefined or empty.
     */
    public String getAttribute( Attributes attributes, String attributeName)
      {
      return attributes.getValue( attributeName);
      }
      
    /**
     * Reports an error if the any of the given attributes are not valid for this element.
     */
    public void validateAttributes( String elementName, Attributes attributes) throws SAXException
      {
      Set<String> validAttributes = getValidAttributes();
      for( int i = attributes.getLength() - 1; i >= 0; i--)
        {
        if( !validAttributes.contains( attributes.getQName(i)))
          {
          throw
            new SAXParseException
            ( "Attribute=" + attributes.getQName(i)
              + " is not allowed for " + elementName + " elements",
              getDocumentLocator());
          }
        }
      }
      
    /**
     * Returns the valid attributes for this element.
     */
    protected Set<String> getValidAttributes()
      {
      return addAttributes( new HashSet<String>());
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      // No attributes to add.
      return attributes;
      }
      
    /**
     * Adds the given attribute list.
     */
    protected Set<String> addAttributeList( Set<String> attributes, String... attributeList)
      {
      Collections.addAll( attributes, attributeList);
      return attributes;
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
   */
  protected class TestCasesHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || FUNCTION_TAG.equals( memberQname);
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
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), SYSTEM_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected Annotated getAnnotated()
      {
      return getSystemTestDef();
      }
    }
  
  /**
   * Handles Function elements.
   *
   */
  protected class FunctionHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || TESTCASE_TAG.equals( memberQname);
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
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected Annotated getAnnotated()
      {
      return getFunctionTestDef();
      }

    private FunctionTestDef functionTestDef_;
    }
  
  /**
   * Base class for annotated elements.
   *
   */
  protected abstract class AnnotatedHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return HAS_TAG.equals( memberQname);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected abstract Annotated getAnnotated();
    
    /**
     * Changes the value of the given annotation.
     */
    public void setAnnotation( String name, String value)
      {
      getAnnotated().setAnnotation( name, value);
      }

    /**
     * Returns the value of the given annotation.
     */
    public String getAnnotation( String name)
      {
      return getAnnotated().getAnnotation( name);
      }
    }
  
  /**
   * Handles Has elements
   */
  protected class HasHandler extends ElementHandler
    {    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      AnnotatedHandler parent = (AnnotatedHandler) getParent();
      try
        {
        String annotation = requireAttribute( attributes, NAME_ATR);
        String value = parent.getAnnotation( annotation);
        if( value != null)
          {
          throw new IllegalArgumentException( "Annotation=" + annotation + " already set to '" + value + "'");
          }
        parent.setAnnotation( annotation, requireAttribute( attributes, VALUE_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add annotation: " + e.getMessage(), getDocumentLocator()); 
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR, VALUE_ATR);
      }
    }
  
  /**
   * Handles TestCase elements.
   *
   */
  protected class TestCaseHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || INPUT_TAG.equals( memberQname);
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
      if( !testCase.getVarBindings().hasNext())
        {
        throw new SAXParseException( "No input specified for test case=" + testCase.getId(), getDocumentLocator()); 
        }
      if( isFailure() && (testCase.getType() == TestCase.Type.SUCCESS))
        {
        throw new SAXParseException( "No failure input specified for test case=" + testCase.getId(), getDocumentLocator()); 
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
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), ID_ATR, FAILURE_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected Annotated getAnnotated()
      {
      return getTestCase();
      }

    private TestCase testCase_;
    private boolean failure_;
    }
  
  /**
   * Handles Input elements.
   *
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
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), TYPE_ATR);
      }

    private String type_;
    }
  
  /**
   * Handles Var elements.
   *
   */
  protected class VarHandler extends AnnotatedHandler
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
          ( "Unexpected failure value=\"" + binding.getValue()
            + "\" for success test case " + testCase.getId(),
            getDocumentLocator()); 
        }
      
      binding.setValueValid( !failure);
      try
        {
        testCase.addVarBinding( binding);
        }
      catch( Exception e)
        {
        throw new SAXParseException( e.getMessage(), getDocumentLocator());
        }

      setVarBinding( binding);
      }

    /**
     * Returns the VarBinding instance for this handler.
     */
    protected void setVarBinding( VarBinding binding)
      {
      binding_ = binding;
      }

    /**
     * Returns the VarBinding instance for this handler.
     */
    public VarBinding getVarBinding()
      {
      return binding_;
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected Annotated getAnnotated()
      {
      return getVarBinding();
      }

    /**
     * Returns the TestCaseHandler for this element.
     */
    public TestCaseHandler getTestCaseHandler()
      {
      return (TestCaseHandler) getParent().getParent();
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR, VALUE_ATR, FAILURE_ATR);
      }

    private VarBinding binding_;
    }
  
  /**
   * Creates a new SystemTestDocReader object.
   */
  public SystemTestDocReader()
    {
    this( null);
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
    stream_ =
      stream == null
      ? System.in
      : stream;
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
      qName.equals( HAS_TAG)?         (ElementHandler) new HasHandler() :
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

    handler.validateAttributes( qName, attributes);
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
