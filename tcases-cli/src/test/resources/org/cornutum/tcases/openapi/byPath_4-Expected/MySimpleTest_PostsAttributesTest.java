package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MySimpleTest_PostsAttributesTest {

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=514682280")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";attributes")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";attributes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";likes=640659892")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=;likes=835347369")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=ltgjsaarufjsntz,39.8,nmjvnsa,-388.5,asyxuquvohcwtz,true;likes=6787962")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=-663.1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;subject=;likes=187325925")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;subject=(/fQ+iXAv6[oToMT*B60a#-lu2U[ 6M~/]G8te^x?ev/ZGRSe'o_7G'lT6n:2 TGq64S?G55LA}?\"nCf4bt)Jda:O-1JP/r\\#*;DZkLr}5v=:[OgEJkwNsAWd#w{=acXgFE}K+}U5Hk5KT'maYikYsS{R$VAC2mS{:D/I~Sd\"%HcE.3j|/& #e2Ak5-H52vY6QA-FsF|~nyK9bCaxM.}DpZ%9,j].;likes=831591488")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .pathParam( "attributes", ";approved=false;likes=803777274;i=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private String forTestServer() {
        return forTestServer( null);
    }

    private String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
