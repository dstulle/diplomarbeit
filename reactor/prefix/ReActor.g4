/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

grammar ReActor;

declaration : 'decl';

event : 'event';

actor : '(' 'class' declaration* event* ')' ;
