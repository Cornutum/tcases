package org.cornutum.examples;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MySimpleTest_PostUseridApprovedTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".1")
                .pathParam( "userId", ".1000")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", "")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".248.1")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".-1")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".0")
                .pathParam( "userId", ".1001")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // userId.Value.Is=1001
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", "")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "approved", ".925787165")
                .pathParam( "userId", ".0")
            .when()
                .request( "DELETE", "/post/{userId}/{approved}")
            .then()
                // approved.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "DELETE", "/post/{userId}/{approved}", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}
