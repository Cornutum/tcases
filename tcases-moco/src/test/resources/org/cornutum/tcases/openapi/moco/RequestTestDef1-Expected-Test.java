package org.cornutum.tcases.openapi.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;
import com.github.dreamhead.moco.SocketServer;
// Test case dependencies

public class RequestTestDef1Test {

    private static SocketServer myServer;

    static {
        myServer = socketServer( 56789);
    }

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.socketRunner( myServer);
    // Test case declarations

    @Test
    public void getPosts_0() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_1() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_2() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_3() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_4() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_5() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }

    @Test
    public void getPosts_6() {
        // Given...
        // When testServer=http://localhost:56789...
        // Then...
    }
    // Test case closing
}
